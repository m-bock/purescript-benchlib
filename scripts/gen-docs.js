import * as fs from "fs";
import * as cp from "child_process";
import { fileURLToPath } from 'url';
import * as path from 'path';

const files = {};



/**
 * Extracts the type signature of a given top-level value from a PureScript module.
 * @param {{file: string, value: string}} param0
 * @returns {string | null} The type signature, or null if not found.
 */
const convertPursVal = ({ file, value, keepValue, keepForall, keepConstraints, keepType }) => {
  const content = fs.readFileSync(path.resolve(file), 'utf8');

  const regex = new RegExp(
    `^(?:foreign import )?${value}\\s*::\\s*(?:(forall\\s+((?:\\w+\\s*)+)\\.\\s*)?(?:(.+?)\\s*=>\\s*)?)?(.+)$`,
    'm'
  );


  const match = content.match(regex);

  if (!match) {
    return null;
  }

  const matchForall = match[1]
  const matchTypeVars = match[2]
  const matchConstraints = match[3]
  const matchType = match[4]

  var ret = [
    keepValue ? [value] : [],
    (keepValue && keepType) ? ["::"] : [],
    keepForall ? [matchForall] : [],
    keepConstraints ? [matchConstraints] : [],
    keepType ? [matchType] : [],
  ].flatMap((x) => x).join(" ")

  return "`" + ret + "`";
}


const convertPursCode = ({ file, section, collapsible, link }) => {
  const filePath = file; // Change this to your actual file
  const fileContent = fs.readFileSync(filePath, "utf8");

  const sectionRegex = /---\s*(\w+)\n([\s\S]*?)(?=\n---\s*\w+|$)/g;
  const sections = {};

  let match;
  while ((match = sectionRegex.exec(fileContent)) !== null) {
    const sectionName = match[1].trim();
    const sectionCode = match[2].trim();
    sections[sectionName] = sectionCode;
  }

  const keys = Object.keys(sections);

  if (files[file] === undefined) {
    files[file] = Object.fromEntries(keys.map((key) => [key, false]));
  }

  files[file][section] = true;

  return [
    link ? "Source Code: " + mkLink(`${file}`, file) : "",
    (collapsible ? mkCollapsible : identity)(
      mkCodeBlock("purescript", sections[section], true)
    ),
  ].join("\n")
};

const convertPursValDef = ({ file, name }) => {
  const fileContent = fs.readFileSync(file, "utf8");

  // Match multiline type signature (indented lines included)
  const typeRegex = new RegExp(
    `^${name}\\s*::[\\s\\S]*?(?=^\\S|\\Z)`,
    "m"
  );

  // Match multiline definition (indented lines included)
  const defRegex = new RegExp(
    `^${name}\\s*=([\\s\\S]*?)(?=^\\S|\\Z)`,
    "m"
  );

  const typeMatch = fileContent.match(typeRegex);
  const defMatch = fileContent.match(defRegex);

  if (!typeMatch || !defMatch) {
    throw new Error(`Definition for '${name}' not found.`);
  }

  const typeDecl = typeMatch[0].trim();
  const valDecl = defMatch[0].trim();

  const all =`\n${typeDecl}\n${valDecl}\n`

  return mkCodeBlock("purescript", all, true)
};


const convertCode = ({ file, link, collapsible, language }) => {
  const fileContent = fs.readFileSync(file, "utf8");
  return [
    link ? "Source Code: " + mkLink(`${file}`, file) : "",
    (collapsible ? mkCollapsible : identity)(
      mkCodeBlock(language || "text", fileContent, true)
    ),
  ].join("\n");
};

const convertRaw = ({ file }) => {
  const filePath = file; // Change this to your actual file
  const fileContent = fs.readFileSync(filePath, "utf8");
  return fileContent;
};

const replaceMustache = (replaceMap, val) => {
  return val.replace(/\{\{([a-zA-Z-0-9_-]+)\}\}/g, (match, key) => {
    const replacement = replaceMap[key];
    if (replacement === undefined) {
      throw new Error(`No replacement found for key: ${key}`);
    }
    return replacement;
  })
};

const convertRun = ({ cmd, hide, hideCmd, text }) => {
  const realCmd = replaceMustache(
    {
      cwd: process.cwd(),
    },
    cmd
  );
  console.error("Running command:", realCmd);
  const stdout = cp.execSync(realCmd, { encoding: "utf8" });
  return [
    hideCmd ? "" : mkCodeBlock("bash", cmd),
    text ? text : "",
    hide ? "" : "\n" + mkCodeBlock("text", removeAnsiColors(stdout)),
  ].join("\n");
};

const mkCollapsible = (content) => {
  return [
    "<details>",
    "<summary>Show/Hide imports</summary>",
    "",
    content,
    "",
    "</details>",
  ].join("\n");
};

const removeAnsiColors = (str) => {
  return str.replace(/\x1B\[[0-9;]*m/g, "");
};

const identity = (val) => val;

const mkLink = (label, link) => {
  return `[${label}](${link})`;
};

const mkCodeBlock = (lang, content, quote) => {
  return ["```" + lang, content.trim(), "```"]
    .join("\n")
    .split("\n")
    .map((line) => (quote ? `> ${line}` : line))
    .join("\n") + "\n";
};

const fns = {
  pursCode: convertPursCode,
  run: convertRun,
  raw: convertRaw,
  code: convertCode,
  pursVal: convertPursVal,
  pursValDef: convertPursValDef
};

const mainFile = (filePath) => {
  const fileContent = fs.readFileSync(filePath, "utf8");

  const updatedContent = fileContent.replace(
    /<!-- start:([a-zA-Z0-9_]+)\s([\s\S]*?)-->[\s\S]*?<!-- end -->/g,
    (_, fnName, jsonString) => {
      console.log(`found replacement`)
      console.log(`fnName=${fnName};`);
      console.log(`jsonString=${jsonString};`);

      const json = JSON.parse(jsonString);
      console.log(`json = ${json}`);
      const convertFn = fns[fnName];
      console.log(`convertFn = ${typeof convertFn === "undefined"}`);
      const converted = convertFn(json);
      console.log(`converted = ${converted}`);

      const jsonPretty = JSON.stringify(json, null, 2);

      return `<!-- start:${fnName} ${jsonPretty} -->${converted}<!-- end -->`;
    }
  );

  fs.writeFileSync(filePath, updatedContent, "utf8");

  Object.entries(files).forEach(([file, sections]) => {
    Object.entries(sections).forEach(([section, value]) => {
      console.log(`File: ${file}, Section: ${section}, Value: ${value}`);
    });
  });
};

const mainFolder = ({folder, filter}) => {
  const files = fs.readdirSync(folder).filter((file) => 
    fs.statSync(path.join(folder, file)).isFile())

  files.forEach((file) => {
    const fileName = path.join(folder, file);
    if (filter && !filter.test(fileName)) return;
      console.log(`Processing file: ${fileName}`);
      mainFile(fileName);
    
  });

};

const main = () => {
  const opts = {
    folder: "docs",
    filter: /.*\.md$/,
  }

  mainFolder(opts);
}

main();

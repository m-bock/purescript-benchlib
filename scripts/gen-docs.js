import * as fs from "fs";
import * as cp from "child_process";
import { fileURLToPath } from 'url';
import * as path from 'path';

const files = {};

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
  ].join("\n");
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
    .join("\n");
};

const fns = {
  pursCode: convertPursCode,
  run: convertRun,
  raw: convertRaw,
  code: convertCode,
};

const mainFile = (filePath) => {
  const fileContent = fs.readFileSync(filePath, "utf8");

  const updatedContent = fileContent.replace(
    /(<!-- start:([a-zA-Z0-9_]+)\n([\s\S]*?)-->)[\s\S]*?(<!-- end -->)/g,
    (_, start, fnName, jsonString, end) => {
      const json = JSON.parse(jsonString);
      const convertFn = fns[fnName];
      const converted = convertFn(json);

      return `${start}\n${converted}\n${end}`;
    }
  );

  fs.writeFileSync(filePath, updatedContent, "utf8");

  Object.entries(files).forEach(([file, sections]) => {
    Object.entries(sections).forEach(([section, value]) => {
      console.log(`File: ${file}, Section: ${section}, Value: ${value}`);
    });
  });
};

const genReadme = (folder, files) => {
  return [
    "# Benchlib Guide",

    "This is a guide to the Benchlib library. It contains various examples and explanations.",

    "## Table of Contents",
    ...files
      .filter((file) => file !== "README.md")
      .map((file) => {
        const fileName = file.replace(/\.md$/, "");
        const h1 = extractH1(path.join(folder, file));
        return `- [${h1}](${file})`;
      }),
  ].join("\n");
}

const extractH1 = (file) => {
  const fileContent = fs.readFileSync(file, "utf8");
  const h1Regex = /^# (.+)$/gm;
  const match = h1Regex.exec(fileContent);
  return match ? match[1] : null;
}

const mainFolder = ({folder, filter}) => {
  const files = fs.readdirSync(folder).filter((file) => 
    fs.statSync(path.join(folder, file)).isFile())

  files.forEach((file) => {
    const fileName = path.join(folder, file);
    if (filter && !filter.test(fileName)) return;
    if (file === "README.md") return;
    
      mainFile(fileName);
    
  });

  const readme = genReadme(folder, files);
  fs.writeFileSync(`${folder}/README.md`, readme, "utf8");
};

const main = () => {
  const opts = {
    folder: "docs/chapters",
    filter: /02/,
  }

  mainFolder(opts);
}

main();

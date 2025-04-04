import * as fs from "fs";
import * as cp from "child_process";

const files = {};

const convertCode = ({ file, section, collapsible, link }) => {
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

const convertRaw = ({ file }) => {
  const filePath = file; // Change this to your actual file
  const fileContent = fs.readFileSync(filePath, "utf8");
  return fileContent;
};

const convertRun = ({ cmd, hide, text }) => {
  const stdout = cp.execSync(cmd, { encoding: "utf8" });
  return [
    mkCodeBlock("bash", cmd),
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
  code: convertCode,
  run: convertRun,
  raw: convertRaw,
};

const mainFile = (filePath) => {
  const fileContent = fs.readFileSync(filePath, "utf8");

  const updatedContent = fileContent.replace(
    /(<!-- start:([a-z]+)\s*\n(\{[^}]+\})\n*-->)[\s\S]*?(<!-- end -->)/g,
    (match, start, fnName, jsonString, end) => {
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

const main = () => {
  const folder = "docs/chapters";

  const files = fs.readdirSync(folder);
  files.forEach((file) => {
    const filePath = `${folder}/${file}`;
    if (fs.statSync(filePath).isFile()) {
      mainFile(filePath);
    }
  });
};

main();

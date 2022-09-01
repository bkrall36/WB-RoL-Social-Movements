# WB-RoL-Social-Movements

Codes for basic stats from Rule of Law and Social Movements in Pakistan Project 

# Replication

The files contain the scripts and datasets used to conduct the analysis presented in the publication "". 

System Requirements: 
* R: 4.x. You can download it <a href="https://cran.r-project.org/bin/windows/base/"> here </a>.
* Python: 3.x. You can download it <a href="https://www.python.org/downloads/"> here </a>.

In order to complete the analysis, follow these steps: 
1. Open the 'masterscript.R' and run the file. Ensure the github_path object is mapped to your computer. 
2. Run the 'Cleaning' R scripts first, then designate which Outputs you would like to run by  specifying 1 or 0. 

The output will be stored in the folder 'Outputs,' which will be populated when you run the individuals 'Code' scripts. 

Overview of Input Data: 
Elections Data - Variable Descriptions
<table>
  <tr>
    <th>Variable</th>
    <th>Description</th>
  </tr>
  <tr>
    <td>Year</td>
    <td>...</td>
  </tr>
   <tr>
    <td>PA ID</td>
    <td>...</td>
  </tr>
    <tr>
    <td>Party Initials</td>
    <td>...</td>
  </tr>
  <tr>
    <td>Vote_Share.1</td>
    <td>...</td>
  </tr>
    <tr>
    <td>treated</td>
    <td>...</td>
  </tr>
    <tr>
    <td>incidental_treatment</td>
    <td>...</td>
  </tr>
    <tr>
    <td>planned_treatment</td>
    <td>...</td>
  </tr>
</table>
  


Important: 
The RoL ELection Analysis.Rmd script can be ran and knitted to display the entire report.

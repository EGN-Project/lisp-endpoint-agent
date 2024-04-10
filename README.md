# EGN Project

### Functionality
The platform is designed for real-time monitoring, employing signature-based detection and machine learning algorithms to identify both known and unknown security threats. It allows for dynamic updates to threat detection rules without the need for software updates. Additionally, the system logs all activities and generates detailed reports for review by engineers.

### Usability Features
The console-based user interface is crafted for ease of use, enabling end-users to initiate manual threat scans and updates. The system provides comprehensive reports in a human-readable format, and users can customize alert levels and response actions based on their preferences.

### Safety Measures
Safety is a top priority, ensuring that the Endpoint Protection Agent's actions do not compromise the integrity and safety of monitored devices. Integration with the blockchain network provides tamper-proof audit trails for all interactions, and the system adheres to best practices for data security and encryption. Regular security audits and code reviews are conducted to address potential vulnerabilities.

### System Design and Key Subsystems
The architecture of the platform includes key subsystems like
  * Endpoint Protection Agent
  * Blockchain Integration Subsystem
  * Smart Contract Development Subsystem
  * User Interface Subsystem
  * Testing and Validation Subsystem

Each subsystem plays a crucial role in ensuring the effectiveness, reliability, and security of the entire Endpoint Protection Platform.

### How To Run
To run Test Network, refer to -> `https://docs.google.com/document/d/1cyQHG-WsbXzv9V11rWc0OUhWlItdNUve1G4DRDz7bSU/edit`

To run Lisp Console, ensure sbcl and quicklisp is installed on your device. Then go in the terminal and run:
  * sbcl
  * (load "path to setup.lisp")  this is found in the quicklisp folder which should be installed locally already
  * (load "path to endpoint-console") 



## Use-case Diagram
<img width="600" src="./meta/use-case-diagram.svg" />

## Class Diagram
<img width="600" src="./meta/class-diagram.svg" />

#### Authors
- [Colton Rohan](https://github.com/ColtonRohan)
- [Emmanuel David](https://github.com/edavid2021)
- [Carlodavid Soto](https://github.com/Bebo561)
- [Vladia Zouga](https://github.com/vladiazouga)
- [Nikhil Shetty](https://github.com/nixz)
- [Yurixander Ricardo](https://github.com/yurixander)

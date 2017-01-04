// Clear out all existing Restagraph schema elements
MATCH (n:rgResource)-[r]->(m:rgResource) DELETE r;
MATCH (n:rgResource)-[r]->(m:rgAttribute) DELETE r;
MATCH (n:rgAttribute) DELETE n;
MATCH (n:rgResource) DELETE n;


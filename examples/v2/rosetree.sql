DROP TABLE rosetree;
DROP TABLE node_branch;
DROP TABLE node;
DROP TABLE branch;

CREATE TABLE branch (
    id serial primary key
);

CREATE TABLE node (
    id serial primary key,
    branch_id int4, -- 0 for root node
    value int4
);

CREATE TABLE node_branch (
    id int4 references node(id),
    next_branch_id int4 references branch(id)
);

CREATE TABLE rosetree (
    id serial primary key,
    root_id int4 references node(id)
);



DROP TABLE rosetree;
DROP TABLE node;
DROP TABLE branch;

CREATE TABLE branch (
    id serial primary key
);

CREATE TABLE node (
    id serial primary key,
    branch_id int4, -- 0 for root node
    next_branch_id int4 references branch(id),
    value int4
);

CREATE TABLE rosetree (
    id serial primary key,
    root_id int4 references node(id)
);



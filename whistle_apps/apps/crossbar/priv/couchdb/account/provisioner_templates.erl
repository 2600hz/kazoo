{
    "_id": "_design/provisioner_templates"
    ,"language": "javascript"
    ,"views": {
        "crossbar_listing": {
            "map": "function(doc) { if (doc.pvt_type != 'provisioner_template' || doc.pvt_deleted) return; emit(doc._id, {'id': doc._id, 'name': doc.name}); }"
        }
    }
    ,"filters": {
        "export":"function(doc, req) { return ( doc.pvt_type == 'provisioner_template' ); }"
    }
}

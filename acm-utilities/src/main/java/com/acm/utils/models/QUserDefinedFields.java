package com.acm.utils.models;

import static com.querydsl.core.types.PathMetadataFactory.forVariable;

import com.querydsl.core.types.PathMetadata;
import javax.annotation.Generated;
import com.querydsl.core.types.Path;
import com.querydsl.core.types.dsl.BooleanPath;
import com.querydsl.core.types.dsl.DateTimePath;
import com.querydsl.core.types.dsl.EntityPathBase;
import com.querydsl.core.types.dsl.NumberPath;
import com.querydsl.core.types.dsl.PathInits;
import com.querydsl.core.types.dsl.SetPath;
import com.querydsl.core.types.dsl.StringPath;

/**
 * QUserDefinedFields is a Querydsl query type for UserDefinedFields.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QUserDefinedFields extends EntityPathBase<UserDefinedFields> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -1769286304L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant userDefinedFields. */
	public static final QUserDefinedFields userDefinedFields =
			new QUserDefinedFields("userDefinedFields");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The date insertion. */
	// inherited
	public final DateTimePath<java.util.Date> dateInsertion = _super.dateInsertion;

	/** The date last update. */
	// inherited
	public final DateTimePath<java.util.Date> dateLastUpdate = _super.dateLastUpdate;

	/** The description. */
	public final StringPath description = createString("description");

	/** The enabled. */
	// inherited
	public final BooleanPath enabled = _super.enabled;

	/** The field masc. */
	public final StringPath fieldMasc = createString("fieldMasc");

	/** The field type. */
	public final NumberPath<Integer> fieldType = createNumber("fieldType", Integer.class);

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The id UDF field. */
	public final NumberPath<Long> idUDFField = createNumber("idUDFField", Long.class);

	/** The id UDF list value. */
	public final NumberPath<Long> idUDFListValue = createNumber("idUDFListValue", Long.class);

	/** The id UDF parent field. */
	public final NumberPath<Long> idUDFParentField = createNumber("idUDFParentField", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The mandatory. */
	public final BooleanPath mandatory = createBoolean("mandatory");

	/** The name. */
	public final StringPath name = createString("name");

	/** The ordre. */
	public final NumberPath<Integer> ordre = createNumber("ordre", Integer.class);

	/** The udf parent field value. */
	public final StringPath udfParentFieldValue = createString("udfParentFieldValue");

	/** The unique field. */
	public final BooleanPath uniqueField = createBoolean("uniqueField");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/** The user defined field group. */
	public final QUserDefinedFieldGroup userDefinedFieldGroup;

	/** The user defined fields links. */
	public final SetPath<UserDefinedFieldsLinks, QUserDefinedFieldsLinks> userDefinedFieldsLinks =
			this.<UserDefinedFieldsLinks, QUserDefinedFieldsLinks>createSet(
					"userDefinedFieldsLinks", UserDefinedFieldsLinks.class,
					QUserDefinedFieldsLinks.class, PathInits.DIRECT2);

	/**
	 * Instantiates a new q user defined fields.
	 *
	 * @param variable the variable
	 */
	public QUserDefinedFields(String variable) {

		this(UserDefinedFields.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new q user defined fields.
	 *
	 * @param path the path
	 */
	public QUserDefinedFields(Path<? extends UserDefinedFields> path) {

		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new q user defined fields.
	 *
	 * @param metadata the metadata
	 */
	public QUserDefinedFields(PathMetadata metadata) {

		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new q user defined fields.
	 *
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QUserDefinedFields(PathMetadata metadata, PathInits inits) {

		this(UserDefinedFields.class, metadata, inits);
	}

	/**
	 * Instantiates a new q user defined fields.
	 *
	 * @param type the type
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QUserDefinedFields(Class<? extends UserDefinedFields> type, PathMetadata metadata,
			PathInits inits) {

		super(type, metadata, inits);
		this.userDefinedFieldGroup = inits.isInitialized("userDefinedFieldGroup")
				? new QUserDefinedFieldGroup(forProperty("userDefinedFieldGroup"))
				: null;
	}

}

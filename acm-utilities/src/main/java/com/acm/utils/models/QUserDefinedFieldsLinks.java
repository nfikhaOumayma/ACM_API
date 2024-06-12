/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import static com.querydsl.core.types.PathMetadataFactory.forVariable;

import javax.annotation.Generated;

import com.querydsl.core.types.Path;
import com.querydsl.core.types.PathMetadata;
import com.querydsl.core.types.dsl.BooleanPath;
import com.querydsl.core.types.dsl.DateTimePath;
import com.querydsl.core.types.dsl.EntityPathBase;
import com.querydsl.core.types.dsl.NumberPath;
import com.querydsl.core.types.dsl.PathInits;
import com.querydsl.core.types.dsl.StringPath;

/**
 * QUserDefinedFieldsLinks is a Querydsl query type for UserDefinedFieldsLinks.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QUserDefinedFieldsLinks extends EntityPathBase<UserDefinedFieldsLinks> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 270788153L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant userDefinedFieldsLinks. */
	public static final QUserDefinedFieldsLinks userDefinedFieldsLinks = new QUserDefinedFieldsLinks(
			"userDefinedFieldsLinks");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The category. */
	public final StringPath category = createString("category");

	/** The customer id. */
	public final NumberPath<Long> customerId = createNumber("customerId", Long.class);

	/** The date insertion. */
	// inherited
	public final DateTimePath<java.util.Date> dateInsertion = _super.dateInsertion;

	/** The date last update. */
	// inherited
	public final DateTimePath<java.util.Date> dateLastUpdate = _super.dateLastUpdate;

	/** The element id. */
	public final NumberPath<Long> elementId = createNumber("elementId", Long.class);

	/** The enabled. */
	// inherited
	public final BooleanPath enabled = _super.enabled;

	/** The field value. */
	public final StringPath fieldValue = createString("fieldValue");

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The id abacus UDF link. */
	public final NumberPath<Long> idAbacusUDFLink = createNumber("idAbacusUDFLink", Long.class);

	/** The index group. */
	public final NumberPath<Long> indexGroup = createNumber("indexGroup", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The loan id. */
	public final NumberPath<Long> loanId = createNumber("loanId", Long.class);

	/** The surveys id. */
	public final NumberPath<Long> surveysId = createNumber("surveysId", Long.class);

	/** The udf list value id. */
	public final NumberPath<Long> udfListValueId = createNumber("udfListValueId", Long.class);

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/** The user defined fields. */
	public final QUserDefinedFields userDefinedFields;

	/**
	 * Instantiates a new q user defined fields links.
	 *
	 * @param variable the variable
	 */
	public QUserDefinedFieldsLinks(String variable) {
		this(UserDefinedFieldsLinks.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new q user defined fields links.
	 *
	 * @param path the path
	 */
	public QUserDefinedFieldsLinks(Path<? extends UserDefinedFieldsLinks> path) {
		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new q user defined fields links.
	 *
	 * @param metadata the metadata
	 */
	public QUserDefinedFieldsLinks(PathMetadata metadata) {
		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new q user defined fields links.
	 *
	 * @param metadata the metadata
	 * @param inits    the inits
	 */
	public QUserDefinedFieldsLinks(PathMetadata metadata, PathInits inits) {
		this(UserDefinedFieldsLinks.class, metadata, inits);
	}

	/**
	 * Instantiates a new q user defined fields links.
	 *
	 * @param type     the type
	 * @param metadata the metadata
	 * @param inits    the inits
	 */
	public QUserDefinedFieldsLinks(Class<? extends UserDefinedFieldsLinks> type, PathMetadata metadata,
			PathInits inits) {
		super(type, metadata, inits);
		this.userDefinedFields = inits.isInitialized("userDefinedFields")
				? new QUserDefinedFields(forProperty("userDefinedFields"), inits.get("userDefinedFields"))
				: null;
	}

}

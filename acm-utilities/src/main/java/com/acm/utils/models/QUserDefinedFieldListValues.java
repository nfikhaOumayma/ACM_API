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
import com.querydsl.core.types.dsl.StringPath;

/**
 * QUserDefinedFieldListValues is a Querydsl query type for UserDefinedFieldListValues.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QUserDefinedFieldListValues extends EntityPathBase<UserDefinedFieldListValues> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -1358623437L;

	/** The Constant userDefinedFieldListValues. */
	public static final QUserDefinedFieldListValues userDefinedFieldListValues =
			new QUserDefinedFieldListValues("userDefinedFieldListValues");

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

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The id UDF list. */
	public final NumberPath<Long> idUDFList = createNumber("idUDFList", Long.class);

	/** The id UDF list link. */
	public final NumberPath<Long> idUDFListLink = createNumber("idUDFListLink", Long.class);

	/** The id UDF list value. */
	public final NumberPath<Long> idUDFListValue = createNumber("idUDFListValue", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The name. */
	public final StringPath name = createString("name");

	/** The parent UDF list value. */
	public final NumberPath<Long> parentUDFListValue =
			createNumber("parentUDFListValue", Long.class);

	/** The score. */
	public final NumberPath<Integer> score = createNumber("score", Integer.class);

	/** The table abacus name. */
	public final StringPath tableAbacusName = createString("tableAbacusName");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q user defined field list values.
	 *
	 * @param variable the variable
	 */
	public QUserDefinedFieldListValues(String variable) {

		super(UserDefinedFieldListValues.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q user defined field list values.
	 *
	 * @param path the path
	 */
	public QUserDefinedFieldListValues(Path<? extends UserDefinedFieldListValues> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q user defined field list values.
	 *
	 * @param metadata the metadata
	 */
	public QUserDefinedFieldListValues(PathMetadata metadata) {

		super(UserDefinedFieldListValues.class, metadata);
	}

}

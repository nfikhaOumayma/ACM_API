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
 * QExpensesType is a Querydsl query type for ExpensesType.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QExpensesType extends EntityPathBase<ExpensesType> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 841704972L;

	/** The Constant expensesType. */
	public static final QExpensesType expensesType = new QExpensesType("expensesType");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The code. */
	public final StringPath code = createString("code");

	/** The date insertion. */
	// inherited
	public final DateTimePath<java.util.Date> dateInsertion = _super.dateInsertion;

	/** The date last update. */
	// inherited
	public final DateTimePath<java.util.Date> dateLastUpdate = _super.dateLastUpdate;

	/** The description. */
	public final StringPath description = createString("description");

	/** The document ID. */
	public final NumberPath<Long> documentID = createNumber("documentID", Long.class);

	/** The document libel. */
	public final StringPath documentLibel = createString("documentLibel");

	/** The enabled. */
	// inherited
	public final BooleanPath enabled = _super.enabled;

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The libel. */
	public final StringPath libel = createString("libel");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q expenses type.
	 *
	 * @param variable the variable
	 */
	public QExpensesType(String variable) {

		super(ExpensesType.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q expenses type.
	 *
	 * @param path the path
	 */
	public QExpensesType(Path<? extends ExpensesType> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q expenses type.
	 *
	 * @param metadata the metadata
	 */
	public QExpensesType(PathMetadata metadata) {

		super(ExpensesType.class, metadata);
	}

}

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
 * QExpensesLimit is a Querydsl query type for ExpensesLimit.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QExpensesLimit extends EntityPathBase<ExpensesLimit> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 315182889L;

	/** The Constant expensesLimit. */
	public static final QExpensesLimit expensesLimit = new QExpensesLimit("expensesLimit");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The cr. */
	public final StringPath cr = createString("cr");

	/** The date insertion. */
	// inherited
	public final DateTimePath<java.util.Date> dateInsertion = _super.dateInsertion;

	/** The date last update. */
	// inherited
	public final DateTimePath<java.util.Date> dateLastUpdate = _super.dateLastUpdate;

	/** The dr. */
	public final StringPath dr = createString("dr");

	/** The enabled. */
	// inherited
	public final BooleanPath enabled = _super.enabled;

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The id branch. */
	public final NumberPath<Long> idBranch = createNumber("idBranch", Long.class);

	/** The id expenses type. */
	public final NumberPath<Long> idExpensesType = createNumber("idExpensesType", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The limit. */
	public final NumberPath<Long> limit = createNumber("limit", Long.class);

	/** The rest limit. */
	public final NumberPath<Long> restLimit = createNumber("restLimit", Long.class);

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q expenses limit.
	 *
	 * @param variable the variable
	 */
	public QExpensesLimit(String variable) {

		super(ExpensesLimit.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q expenses limit.
	 *
	 * @param path the path
	 */
	public QExpensesLimit(Path<? extends ExpensesLimit> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q expenses limit.
	 *
	 * @param metadata the metadata
	 */
	public QExpensesLimit(PathMetadata metadata) {

		super(ExpensesLimit.class, metadata);
	}

}

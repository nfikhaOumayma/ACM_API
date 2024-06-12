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
 * QExpenses is a Querydsl query type for Expenses.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QExpenses extends EntityPathBase<Expenses> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1534364594L;

	/** The Constant expenses. */
	public static final QExpenses expenses = new QExpenses("expenses");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The apply date. */
	public final DateTimePath<java.util.Date> applyDate =
			createDateTime("applyDate", java.util.Date.class);

	/** The balance. */
	public final NumberPath<Long> balance = createNumber("balance", Long.class);

	/** The branch description. */
	public final StringPath branchDescription = createString("branchDescription");

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

	/** The expenses amount. */
	public final NumberPath<Long> expensesAmount = createNumber("expensesAmount", Long.class);

	/** The expenses type libelle. */
	public final StringPath expensesTypeLibelle = createString("expensesTypeLibelle");

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The id branch. */
	public final NumberPath<Long> idBranch = createNumber("idBranch", Long.class);

	/** The id expenses type. */
	public final NumberPath<Long> idExpensesType = createNumber("idExpensesType", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The note. */
	public final StringPath note = createString("note");

	/** The owner. */
	public final StringPath owner = createString("owner");

	/** The owner name. */
	public final StringPath ownerName = createString("ownerName");

	/** The statut. */
	public final NumberPath<Integer> statut = createNumber("statut", Integer.class);

	/** The teller. */
	public final StringPath teller = createString("teller");

	/** The teller name. */
	public final StringPath tellerName = createString("tellerName");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q expenses.
	 *
	 * @param variable the variable
	 */
	public QExpenses(String variable) {

		super(Expenses.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q expenses.
	 *
	 * @param path the path
	 */
	public QExpenses(Path<? extends Expenses> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q expenses.
	 *
	 * @param metadata the metadata
	 */
	public QExpenses(PathMetadata metadata) {

		super(Expenses.class, metadata);
	}

}

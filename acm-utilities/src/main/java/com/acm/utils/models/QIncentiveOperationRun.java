/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import static com.querydsl.core.types.PathMetadataFactory.forVariable;

import javax.annotation.Generated;

import com.querydsl.core.types.Path;
import com.querydsl.core.types.PathMetadata;
import com.querydsl.core.types.dsl.DateTimePath;
import com.querydsl.core.types.dsl.EntityPathBase;
import com.querydsl.core.types.dsl.NumberPath;
import com.querydsl.core.types.dsl.StringPath;

/**
 * QIncentiveOperationRun is a Querydsl query type for IncentiveOperationRun.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QIncentiveOperationRun extends EntityPathBase<IncentiveOperationRun> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 266661240L;

	/** The Constant incentiveOperationRun. */
	public static final QIncentiveOperationRun incentiveOperationRun =
			new QIncentiveOperationRun("incentiveOperationRun");

	/** The branch. */
	public final StringPath branch = createString("branch");

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The incentive type MEL. */
	public final StringPath incentiveTypeMEL = createString("incentiveTypeMEL");

	/** The incentive type VSE. */
	public final StringPath incentiveTypeVSE = createString("incentiveTypeVSE");

	/** The incentive value. */
	public final NumberPath<Long> incentiveValue = createNumber("incentiveValue", Long.class);

	/** The issue loan month MEL. */
	public final NumberPath<Long> issueLoanMonthMEL = createNumber("issueLoanMonthMEL", Long.class);

	/** The issue loan month VSE. */
	public final NumberPath<Long> issueLoanMonthVSE = createNumber("issueLoanMonthVSE", Long.class);

	/** The loan officer name. */
	public final StringPath loanOfficerName = createString("loanOfficerName");

	/** The month. */
	public final StringPath month = createString("month");

	/** The report name. */
	public final StringPath reportName = createString("reportName");

	/** The role. */
	public final StringPath role = createString("role");

	/** The run date. */
	public final DateTimePath<java.util.Date> runDate =
			createDateTime("runDate", java.util.Date.class);

	/** The total loan amount MEL. */
	public final NumberPath<Long> totalLoanAmountMEL =
			createNumber("totalLoanAmountMEL", Long.class);

	/** The total loan amount VSE. */
	public final NumberPath<Long> totalLoanAmountVSE =
			createNumber("totalLoanAmountVSE", Long.class);

	/** The user name. */
	public final StringPath userName = createString("userName");

	/**
	 * Instantiates a new q incentive operation run.
	 *
	 * @param variable the variable
	 */
	public QIncentiveOperationRun(String variable) {

		super(IncentiveOperationRun.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q incentive operation run.
	 *
	 * @param path the path
	 */
	public QIncentiveOperationRun(Path<? extends IncentiveOperationRun> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q incentive operation run.
	 *
	 * @param metadata the metadata
	 */
	public QIncentiveOperationRun(PathMetadata metadata) {

		super(IncentiveOperationRun.class, metadata);
	}

}

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
 * QIncentiveRegistrationRun is a Querydsl query type for IncentiveRegistrationRun.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QIncentiveRegistrationRun extends EntityPathBase<IncentiveRegistrationRun> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -153230146L;

	/** The Constant incentiveRegistrationRun. */
	public static final QIncentiveRegistrationRun incentiveRegistrationRun =
			new QIncentiveRegistrationRun("incentiveRegistrationRun");

	/** The branch. */
	public final StringPath branch = createString("branch");

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The incentive new customer MEL. */
	public final NumberPath<Long> incentiveNewCustomerMEL =
			createNumber("incentiveNewCustomerMEL", Long.class);

	/** The incentive new customer VSE. */
	public final NumberPath<Long> incentiveNewCustomerVSE =
			createNumber("incentiveNewCustomerVSE", Long.class);

	/** The incentive re newal customer MEL. */
	public final NumberPath<Long> incentiveReNewalCustomerMEL =
			createNumber("incentiveReNewalCustomerMEL", Long.class);

	/** The incentive re newal customer VSE. */
	public final NumberPath<Long> incentiveReNewalCustomerVSE =
			createNumber("incentiveReNewalCustomerVSE", Long.class);

	/** The incentive value. */
	public final NumberPath<Long> incentiveValue = createNumber("incentiveValue", Long.class);

	/** The loan officer name. */
	public final StringPath loanOfficerName = createString("loanOfficerName");

	/** The month. */
	public final StringPath month = createString("month");

	/** The new customer MEL. */
	public final NumberPath<Long> newCustomerMEL = createNumber("newCustomerMEL", Long.class);

	/** The new customer VSE. */
	public final NumberPath<Long> newCustomerVSE = createNumber("newCustomerVSE", Long.class);

	/** The re newal customer MEL. */
	public final NumberPath<Long> reNewalCustomerMEL =
			createNumber("reNewalCustomerMEL", Long.class);

	/** The re newal customer VSE. */
	public final NumberPath<Long> reNewalCustomerVSE =
			createNumber("reNewalCustomerVSE", Long.class);

	/** The report name. */
	public final StringPath reportName = createString("reportName");

	/** The run date. */
	public final DateTimePath<java.util.Date> runDate =
			createDateTime("runDate", java.util.Date.class);

	/** The setting incentive new customer MEL. */
	public final NumberPath<Long> settingIncentiveNewCustomerMEL =
			createNumber("settingIncentiveNewCustomerMEL", Long.class);

	/** The setting incentive new customer VSE. */
	public final NumberPath<Long> settingIncentiveNewCustomerVSE =
			createNumber("settingIncentiveNewCustomerVSE", Long.class);

	/** The setting incentive re newal customer MEL. */
	public final NumberPath<Long> settingIncentiveReNewalCustomerMEL =
			createNumber("settingIncentiveReNewalCustomerMEL", Long.class);

	/** The setting incentive re newal customer VSE. */
	public final NumberPath<Long> settingIncentiveReNewalCustomerVSE =
			createNumber("settingIncentiveReNewalCustomerVSE", Long.class);

	/** The total customer MEL. */
	public final NumberPath<Long> totalCustomerMEL = createNumber("totalCustomerMEL", Long.class);

	/** The total customer VSE. */
	public final NumberPath<Long> totalCustomerVSE = createNumber("totalCustomerVSE", Long.class);

	/** The user name. */
	public final StringPath userName = createString("userName");

	/**
	 * Instantiates a new q incentive registration run.
	 *
	 * @param variable the variable
	 */
	public QIncentiveRegistrationRun(String variable) {

		super(IncentiveRegistrationRun.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q incentive registration run.
	 *
	 * @param path the path
	 */
	public QIncentiveRegistrationRun(Path<? extends IncentiveRegistrationRun> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q incentive registration run.
	 *
	 * @param metadata the metadata
	 */
	public QIncentiveRegistrationRun(PathMetadata metadata) {

		super(IncentiveRegistrationRun.class, metadata);
	}

}

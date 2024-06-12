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
 * QIBLoan is a Querydsl query type for IBLoan.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QIBLoan extends EntityPathBase<IBLoan> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -1852136480L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant iBLoan. */
	public static final QIBLoan iBLoan = new QIBLoan("iBLoan");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The apply amount total. */
	public final NumberPath<java.math.BigDecimal> applyAmountTotal =
			createNumber("applyAmountTotal", java.math.BigDecimal.class);

	/** The apply date. */
	public final DateTimePath<java.util.Date> applyDate =
			createDateTime("applyDate", java.util.Date.class);

	/** The branch description. */
	public final StringPath branchDescription = createString("branchDescription");

	/** The branch ID. */
	public final NumberPath<Integer> branchID = createNumber("branchID", Integer.class);

	/** The branch name. */
	public final StringPath branchName = createString("branchName");

	/** The currency decimal places. */
	public final NumberPath<Integer> currencyDecimalPlaces =
			createNumber("currencyDecimalPlaces", Integer.class);

	/** The currency symbol. */
	public final StringPath currencySymbol = createString("currencySymbol");

	/** The customer. */
	public final QCustomer customer;

	/** The customer name. */
	public final StringPath customerName = createString("customerName");

	/** The date insertion. */
	// inherited
	public final DateTimePath<java.util.Date> dateInsertion = _super.dateInsertion;

	/** The date last update. */
	// inherited
	public final DateTimePath<java.util.Date> dateLastUpdate = _super.dateLastUpdate;

	/** The enabled. */
	// inherited
	public final BooleanPath enabled = _super.enabled;

	/** The grace period. */
	public final NumberPath<Integer> gracePeriod = createNumber("gracePeriod", Integer.class);

	/** The id ib loan. */
	public final NumberPath<Long> idIbLoan = createNumber("idIbLoan", Long.class);

	/** The id loan extern. */
	public final NumberPath<Long> idLoanExtern = createNumber("idLoanExtern", Long.class);

	/** The initial payment date. */
	public final DateTimePath<java.util.Date> initialPaymentDate =
			createDateTime("initialPaymentDate", java.util.Date.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The issue date. */
	public final DateTimePath<java.util.Date> issueDate =
			createDateTime("issueDate", java.util.Date.class);

	/** The issue fee amount. */
	public final NumberPath<java.math.BigDecimal> issueFeeAmount =
			createNumber("issueFeeAmount", java.math.BigDecimal.class);

	/** The loan type. */
	public final StringPath loanType = createString("loanType");

	/** The normal payment. */
	public final NumberPath<Long> normalPayment = createNumber("normalPayment", Long.class);

	/** The note. */
	public final StringPath note = createString("note");

	/** The owner. */
	public final StringPath owner = createString("owner");

	/** The owner name. */
	public final StringPath ownerName = createString("ownerName");

	/** The payment freq. */
	public final NumberPath<Integer> paymentFreq = createNumber("paymentFreq", Integer.class);

	/** The periods deferred. */
	public final NumberPath<Integer> periodsDeferred =
			createNumber("periodsDeferred", Integer.class);

	/** The portfolio code. */
	public final StringPath portfolioCode = createString("portfolioCode");

	/** The portfolio description. */
	public final StringPath portfolioDescription = createString("portfolioDescription");

	/** The portfolio id. */
	public final NumberPath<Long> portfolioId = createNumber("portfolioId", Long.class);

	/** The product code. */
	public final StringPath productCode = createString("productCode");

	/** The product description. */
	public final StringPath productDescription = createString("productDescription");

	/** The product id. */
	public final NumberPath<Integer> productId = createNumber("productId", Integer.class);

	/** The project description. */
	public final StringPath projectDescription = createString("projectDescription");

	/** The statut. */
	public final NumberPath<Integer> statut = createNumber("statut", Integer.class);

	/** The term period ID. */
	public final NumberPath<Long> termPeriodID = createNumber("termPeriodID", Long.class);

	/** The term period num. */
	public final NumberPath<Integer> termPeriodNum = createNumber("termPeriodNum", Integer.class);

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new QIB loan.
	 *
	 * @param variable the variable
	 */
	public QIBLoan(String variable) {

		this(IBLoan.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new QIB loan.
	 *
	 * @param path the path
	 */
	public QIBLoan(Path<? extends IBLoan> path) {

		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new QIB loan.
	 *
	 * @param metadata the metadata
	 */
	public QIBLoan(PathMetadata metadata) {

		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new QIB loan.
	 *
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QIBLoan(PathMetadata metadata, PathInits inits) {

		this(IBLoan.class, metadata, inits);
	}

	/**
	 * Instantiates a new QIB loan.
	 *
	 * @param type the type
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QIBLoan(Class<? extends IBLoan> type, PathMetadata metadata, PathInits inits) {

		super(type, metadata, inits);
		this.customer =
				inits.isInitialized("customer") ? new QCustomer(forProperty("customer")) : null;
	}

}

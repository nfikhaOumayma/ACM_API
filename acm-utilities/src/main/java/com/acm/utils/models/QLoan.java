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
import com.querydsl.core.types.dsl.SetPath;
import com.querydsl.core.types.dsl.StringPath;

/**
 * QLoan is a Querydsl query type for Loan.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QLoan extends EntityPathBase<Loan> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1924460807L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant loan. */
	public static final QLoan loan = new QLoan("loan");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The account number extern. */
	public final StringPath accountNumberExtern = createString("accountNumberExtern");

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The apply amount total. */
	public final NumberPath<java.math.BigDecimal> applyAmountTotal =
			createNumber("applyAmountTotal", java.math.BigDecimal.class);

	/** The apply date. */
	public final DateTimePath<java.util.Date> applyDate =
			createDateTime("applyDate", java.util.Date.class);

	/** The approvel amount. */
	public final NumberPath<java.math.BigDecimal> approvelAmount =
			createNumber("approvelAmount", java.math.BigDecimal.class);

	/** The apr. */
	public final NumberPath<java.math.BigDecimal> apr =
			createNumber("apr", java.math.BigDecimal.class);

	/** The assign customer. */
	public final BooleanPath assignCustomer = createBoolean("assignCustomer");

	/** The branch description. */
	public final StringPath branchDescription = createString("branchDescription");

	/** The branch ID. */
	public final NumberPath<Integer> branchID = createNumber("branchID", Integer.class);

	/** The branch name. */
	public final StringPath branchName = createString("branchName");

	/** The calculate initial payment date. */
	public final BooleanPath calculateInitialPaymentDate =
			createBoolean("calculateInitialPaymentDate");

	/** The category. */
	public final StringPath category = createString("category");

	/** The change date status workflow. */
	public final DateTimePath<java.util.Date> changeDateStatusWorkflow =
			createDateTime("changeDateStatusWorkflow", java.util.Date.class);

	/** The community CU loan ID. */
	public final NumberPath<Long> communityCULoanID = createNumber("communityCULoanID", Long.class);

	/** The creation date. */
	public final DateTimePath<java.util.Date> creationDate =
			createDateTime("creationDate", java.util.Date.class);

	/** The currency decimal places. */
	public final NumberPath<Integer> currencyDecimalPlaces =
			createNumber("currencyDecimalPlaces", Integer.class);

	/** The currency symbol. */
	public final StringPath currencySymbol = createString("currencySymbol");

	/** The customer. */
	public final QCustomer customer;

	/** The customer id. */
	public final NumberPath<Long> customerId = createNumber("customerId", Long.class);

	/** The customer name. */
	public final StringPath customerName = createString("customerName");

	/** The customer type. */
	public final StringPath customerType = createString("customerType");

	/** The date insertion. */
	// inherited
	public final DateTimePath<java.util.Date> dateInsertion = _super.dateInsertion;

	/** The date last update. */
	// inherited
	public final DateTimePath<java.util.Date> dateLastUpdate = _super.dateLastUpdate;

	/** The district code id. */
	public final NumberPath<Integer> districtCodeId = createNumber("districtCodeId", Integer.class);

	/** The effective int rate. */
	public final NumberPath<java.math.BigDecimal> effectiveIntRate =
			createNumber("effectiveIntRate", java.math.BigDecimal.class);

	/** The enabled. */
	// inherited
	public final BooleanPath enabled = _super.enabled;

	/** The etape workflow. */
	public final NumberPath<Integer> etapeWorkflow = createNumber("etapeWorkflow", Integer.class);

	/** The fee amt 1. */
	public final NumberPath<java.math.BigDecimal> feeAmt1 =
			createNumber("feeAmt1", java.math.BigDecimal.class);

	/** The grace period. */
	public final NumberPath<Integer> gracePeriod = createNumber("gracePeriod", Integer.class);

	/** The group owner. */
	public final StringPath groupOwner = createString("groupOwner");

	/** The group owner name. */
	public final StringPath groupOwnerName = createString("groupOwnerName");

	/** The guarantor source id. */
	public final NumberPath<Integer> guarantorSourceId =
			createNumber("guarantorSourceId", Integer.class);

	/** The id account extern. */
	public final NumberPath<Long> idAccountExtern = createNumber("idAccountExtern", Long.class);

	/** The id ib loan. */
	public final NumberPath<Long> idIbLoan = createNumber("idIbLoan", Long.class);

	/** The id loan. */
	public final NumberPath<Long> idLoan = createNumber("idLoan", Long.class);

	/** The id loan extern. */
	public final NumberPath<Long> idLoanExtern = createNumber("idLoanExtern", Long.class);

	/** The ignore odd days. */
	public final BooleanPath ignoreOddDays = createBoolean("ignoreOddDays");

	/** The ihm root. */
	public final StringPath ihmRoot = createString("ihmRoot");

	/** The industry code. */
	public final StringPath industryCode = createString("industryCode");

	/** The industry code description. */
	public final StringPath industryCodeDescription = createString("industryCodeDescription");

	/** The initial payment date. */
	public final DateTimePath<java.util.Date> initialPaymentDate =
			createDateTime("initialPaymentDate", java.util.Date.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The installment number. */
	public final NumberPath<Integer> installmentNumber =
			createNumber("installmentNumber", Integer.class);

	/** The interest freq. */
	public final NumberPath<Integer> interestFreq = createNumber("interestFreq", Integer.class);

	/** The int pay period num. */
	public final NumberPath<Integer> intPayPeriodNum =
			createNumber("intPayPeriodNum", Integer.class);

	/** The issue date. */
	public final DateTimePath<java.util.Date> issueDate =
			createDateTime("issueDate", java.util.Date.class);

	/** The issue fee amount. */
	public final NumberPath<java.math.BigDecimal> issueFeeAmount =
			createNumber("issueFeeAmount", java.math.BigDecimal.class);

	/** The loan application status. */
	public final StringPath loanApplicationStatus = createString("loanApplicationStatus");

	/** The loan assets. */
	public final SetPath<AssetLoan, QAssetLoan> loanAssets = this.<AssetLoan, QAssetLoan>createSet(
			"loanAssets", AssetLoan.class, QAssetLoan.class, PathInits.DIRECT2);

	/** The loan calculation mode. */
	public final NumberPath<Integer> loanCalculationMode =
			createNumber("loanCalculationMode", Integer.class);

	/** The loan instances. */
	public final SetPath<LoanInstance, QLoanInstance> loanInstances =
			this.<LoanInstance, QLoanInstance>createSet("loanInstances", LoanInstance.class,
					QLoanInstance.class, PathInits.DIRECT2);

	/** The loan reason code. */
	public final StringPath loanReasonCode = createString("loanReasonCode");

	/** The loan reason description. */
	public final StringPath loanReasonDescription = createString("loanReasonDescription");

	/** The normal payment. */
	public final NumberPath<Long> normalPayment = createNumber("normalPayment", Long.class);

	/** The note. */
	public final StringPath note = createString("note");

	/** The opening balance. */
	public final NumberPath<Integer> openingBalance = createNumber("openingBalance", Integer.class);

	/** The other informations. */
	public final StringPath otherInformations = createString("otherInformations");

	/** The owner. */
	public final StringPath owner = createString("owner");

	/** The owner name. */
	public final StringPath ownerName = createString("ownerName");

	/** The parent id. */
	public final NumberPath<Long> parentId = createNumber("parentId", Long.class);

	/** The payment freq. */
	public final NumberPath<Integer> paymentFreq = createNumber("paymentFreq", Integer.class);

	/** The periods deferred. */
	public final NumberPath<Integer> periodsDeferred =
			createNumber("periodsDeferred", Integer.class);

	/** The periods deferred type. */
	public final NumberPath<Integer> periodsDeferredType =
			createNumber("periodsDeferredType", Integer.class);

	/** The personal contribution. */
	public final NumberPath<Integer> personalContribution =
			createNumber("personalContribution", Integer.class);

	/** The portfolio code. */
	public final StringPath portfolioCode = createString("portfolioCode");

	/** The portfolio description. */
	public final StringPath portfolioDescription = createString("portfolioDescription");

	/** The portfolio id. */
	public final NumberPath<Long> portfolioId = createNumber("portfolioId", Long.class);

	/** The process instance id. */
	public final StringPath processInstanceId = createString("processInstanceId");

	/** The process name. */
	public final StringPath processName = createString("processName");

	/** The product code. */
	public final StringPath productCode = createString("productCode");

	/** The product description. */
	public final StringPath productDescription = createString("productDescription");

	/** The product id. */
	public final NumberPath<Integer> productId = createNumber("productId", Integer.class);

	/** The product rate. */
	public final NumberPath<java.math.BigDecimal> productRate =
			createNumber("productRate", java.math.BigDecimal.class);

	/** The ready for disb. */
	public final NumberPath<Integer> readyForDisb = createNumber("readyForDisb", Integer.class);

	/** The refinance reason id. */
	public final NumberPath<Integer> refinanceReasonId =
			createNumber("refinanceReasonId", Integer.class);

	/** The review from. */
	public final NumberPath<Long> reviewFrom = createNumber("reviewFrom", Long.class);

	/** The source of funds ID. */
	public final NumberPath<Integer> sourceOfFundsID =
			createNumber("sourceOfFundsID", Integer.class);

	/** The statut. */
	public final NumberPath<Integer> statut = createNumber("statut", Integer.class);

	/** The statut libelle. */
	public final StringPath statutLibelle = createString("statutLibelle");

	/** The statut workflow. */
	public final NumberPath<Integer> statutWorkflow = createNumber("statutWorkflow", Integer.class);

	/** The term period ID. */
	public final NumberPath<Long> termPeriodID = createNumber("termPeriodID", Long.class);

	/** The term period num. */
	public final NumberPath<Integer> termPeriodNum = createNumber("termPeriodNum", Integer.class);

	/** The total interest. */
	public final NumberPath<java.math.BigDecimal> totalInterest =
			createNumber("totalInterest", java.math.BigDecimal.class);

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/** The update loan. */
	public final BooleanPath updateLoan = createBoolean("updateLoan");

	/** The workflow completed. */
	public final BooleanPath workflowCompleted = createBoolean("workflowCompleted");

	/**
	 * Instantiates a new q loan.
	 *
	 * @param variable the variable
	 */
	public QLoan(String variable) {

		this(Loan.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new q loan.
	 *
	 * @param path the path
	 */
	public QLoan(Path<? extends Loan> path) {

		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new q loan.
	 *
	 * @param metadata the metadata
	 */
	public QLoan(PathMetadata metadata) {

		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new q loan.
	 *
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QLoan(PathMetadata metadata, PathInits inits) {

		this(Loan.class, metadata, inits);
	}

	/**
	 * Instantiates a new q loan.
	 *
	 * @param type the type
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QLoan(Class<? extends Loan> type, PathMetadata metadata, PathInits inits) {

		super(type, metadata, inits);
		this.customer =
				inits.isInitialized("customer") ? new QCustomer(forProperty("customer")) : null;
	}

}

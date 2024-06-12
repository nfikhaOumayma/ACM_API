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
 * QAcmCollateral is a Querydsl query type for AcmCollateral.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QAcmCollateral extends EntityPathBase<AcmCollateral> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -1717695765L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant acmCollateral. */
	public static final QAcmCollateral acmCollateral = new QAcmCollateral("acmCollateral");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The collateral type description. */
	public final StringPath collateralTypeDescription = createString("collateralTypeDescription");

	/** The collateral type id extern. */
	public final NumberPath<Long> collateralTypeIdExtern =
			createNumber("collateralTypeIdExtern", Long.class);

	/** The customer. */
	public final QCustomer customer;

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

	/** The expiry date. */
	public final DateTimePath<java.util.Date> expiryDate =
			createDateTime("expiryDate", java.util.Date.class);

	/** The extern customer id. */
	public final NumberPath<Long> externCustomerId = createNumber("externCustomerId", Long.class);

	/** The extern loan id. */
	public final NumberPath<Long> externLoanId = createNumber("externLoanId", Long.class);

	/** The fixed cost. */
	public final NumberPath<java.math.BigDecimal> fixedCost =
			createNumber("fixedCost", java.math.BigDecimal.class);

	/** The gross value. */
	public final NumberPath<java.math.BigDecimal> grossValue =
			createNumber("grossValue", java.math.BigDecimal.class);

	/** The id account extern. */
	public final NumberPath<Long> idAccountExtern = createNumber("idAccountExtern", Long.class);

	/** The id acm collateral. */
	public final NumberPath<Long> idAcmCollateral = createNumber("idAcmCollateral", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The is deleted. */
	public final BooleanPath isDeleted = createBoolean("isDeleted");

	/** The loan. */
	public final QLoan loan;

	/** The net value. */
	public final NumberPath<java.math.BigDecimal> netValue =
			createNumber("netValue", java.math.BigDecimal.class);

	/** The original gross value. */
	public final NumberPath<java.math.BigDecimal> originalGrossValue =
			createNumber("originalGrossValue", java.math.BigDecimal.class);

	/** The realised value. */
	public final NumberPath<java.math.BigDecimal> realisedValue =
			createNumber("realisedValue", java.math.BigDecimal.class);

	/** The reference. */
	public final StringPath reference = createString("reference");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/** The value date. */
	public final DateTimePath<java.util.Date> valueDate =
			createDateTime("valueDate", java.util.Date.class);

	/** The valuer id. */
	public final StringPath valuerId = createString("valuerId");

	/** The valuer name. */
	public final StringPath valuerName = createString("valuerName");

	/** The with holding rate. */
	public final NumberPath<java.math.BigDecimal> withHoldingRate =
			createNumber("withHoldingRate", java.math.BigDecimal.class);

	/**
	 * Instantiates a new q acm collateral.
	 *
	 * @param variable the variable
	 */
	public QAcmCollateral(String variable) {

		this(AcmCollateral.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new q acm collateral.
	 *
	 * @param path the path
	 */
	public QAcmCollateral(Path<? extends AcmCollateral> path) {

		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new q acm collateral.
	 *
	 * @param metadata the metadata
	 */
	public QAcmCollateral(PathMetadata metadata) {

		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new q acm collateral.
	 *
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QAcmCollateral(PathMetadata metadata, PathInits inits) {

		this(AcmCollateral.class, metadata, inits);
	}

	/**
	 * Instantiates a new q acm collateral.
	 *
	 * @param type the type
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QAcmCollateral(Class<? extends AcmCollateral> type, PathMetadata metadata,
			PathInits inits) {

		super(type, metadata, inits);
		this.customer =
				inits.isInitialized("customer") ? new QCustomer(forProperty("customer")) : null;
		this.loan = inits.isInitialized("loan") ? new QLoan(forProperty("loan"), inits.get("loan"))
				: null;
	}

}

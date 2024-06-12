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
 * QAcmCreditLine is a Querydsl query type for AcmCreditLine.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QAcmCreditLine extends EntityPathBase<AcmCreditLine> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -441676991L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant acmCreditLine. */
	public static final QAcmCreditLine acmCreditLine = new QAcmCreditLine("acmCreditLine");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The balance. */
	public final NumberPath<java.math.BigDecimal> balance =
			createNumber("balance", java.math.BigDecimal.class);

	/** The control balance. */
	public final BooleanPath controlBalance = createBoolean("controlBalance");

	/** The date insertion. */
	public final DateTimePath<java.util.Date> dateInsertion = _super.dateInsertion;

	/** The date last update. */
	public final DateTimePath<java.util.Date> dateLastUpdate = _super.dateLastUpdate;

	/** The description. */
	public final StringPath description = createString("description");

	/** The enabled. */
	public final BooleanPath enabled = _super.enabled;

	/** The expiry date. */
	public final DateTimePath<java.util.Date> expiryDate =
			createDateTime("expiryDate", java.util.Date.class);

	/** The fund name. */
	public final StringPath fundName = createString("fundName");

	/** The fund priority. */
	public final NumberPath<Long> fundPriority = createNumber("fundPriority", Long.class);

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The insert by. */
	public final StringPath insertBy = _super.insertBy;

	/** The issue date. */
	public final DateTimePath<java.util.Date> issueDate =
			createDateTime("issueDate", java.util.Date.class);

	/** The products. */
	public final SetPath<Product, QProduct> products = this.<Product, QProduct>createSet("products",
			Product.class, QProduct.class, PathInits.DIRECT2);

	/** The third party. */
	public final QAcmThirdParty thirdParty;

	/** The topped up histories. */
	public final SetPath<AcmToppedUpHistory, QAcmToppedUpHistory> toppedUpHistories =
			this.<AcmToppedUpHistory, QAcmToppedUpHistory>createSet("toppedUpHistories",
					AcmToppedUpHistory.class, QAcmToppedUpHistory.class, PathInits.DIRECT2);

	/** The updated by. */
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q acm credit line.
	 *
	 * @param variable the variable
	 */
	public QAcmCreditLine(String variable) {

		this(AcmCreditLine.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new q acm credit line.
	 *
	 * @param path the path
	 */
	public QAcmCreditLine(Path<? extends AcmCreditLine> path) {

		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new q acm credit line.
	 *
	 * @param metadata the metadata
	 */
	public QAcmCreditLine(PathMetadata metadata) {

		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new q acm credit line.
	 *
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QAcmCreditLine(PathMetadata metadata, PathInits inits) {

		this(AcmCreditLine.class, metadata, inits);
	}

	/**
	 * Instantiates a new q acm credit line.
	 *
	 * @param type the type
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QAcmCreditLine(Class<? extends AcmCreditLine> type, PathMetadata metadata,
			PathInits inits) {

		super(type, metadata, inits);
		this.thirdParty =
				inits.isInitialized("thirdParty") ? new QAcmThirdParty(forProperty("thirdParty"))
						: null;
	}

}

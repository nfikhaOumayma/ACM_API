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
 * QAcmToppedUpHistory is a Querydsl query type for AcmToppedUpHistory.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QAcmToppedUpHistory extends EntityPathBase<AcmToppedUpHistory> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -52239669L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant acmToppedUpHistory. */
	public static final QAcmToppedUpHistory acmToppedUpHistory =
			new QAcmToppedUpHistory("acmToppedUpHistory");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The amount. */
	public final NumberPath<java.math.BigDecimal> amount =
			createNumber("amount", java.math.BigDecimal.class);

	/** The credit line. */
	public final QAcmCreditLine creditLine;

	/** The date insertion. */
	public final DateTimePath<java.util.Date> dateInsertion = _super.dateInsertion;

	/** The date last update. */
	public final DateTimePath<java.util.Date> dateLastUpdate = _super.dateLastUpdate;

	/** The enabled. */
	public final BooleanPath enabled = _super.enabled;

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The insert by. */
	public final StringPath insertBy = _super.insertBy;

	/** The issue date. */
	public final DateTimePath<java.util.Date> issueDate =
			createDateTime("issueDate", java.util.Date.class);

	/** The updated by. */
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q acm topped up history.
	 *
	 * @param variable the variable
	 */
	public QAcmToppedUpHistory(String variable) {

		this(AcmToppedUpHistory.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new q acm topped up history.
	 *
	 * @param path the path
	 */
	public QAcmToppedUpHistory(Path<? extends AcmToppedUpHistory> path) {

		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new q acm topped up history.
	 *
	 * @param metadata the metadata
	 */
	public QAcmToppedUpHistory(PathMetadata metadata) {

		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new q acm topped up history.
	 *
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QAcmToppedUpHistory(PathMetadata metadata, PathInits inits) {

		this(AcmToppedUpHistory.class, metadata, inits);
	}

	/**
	 * Instantiates a new q acm topped up history.
	 *
	 * @param type the type
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QAcmToppedUpHistory(Class<? extends AcmToppedUpHistory> type, PathMetadata metadata,
			PathInits inits) {

		super(type, metadata, inits);
		this.creditLine = inits.isInitialized("creditLine")
				? new QAcmCreditLine(forProperty("creditLine"), inits.get("creditLine"))
				: null;
	}

}

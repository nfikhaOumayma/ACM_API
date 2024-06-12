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
 * QChargeFees is a Querydsl query type for ChargeFees.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QChargeFees extends EntityPathBase<ChargeFees> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -1335852712L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant chargeFees. */
	public static final QChargeFees chargeFees = new QChargeFees("chargeFees");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The amount. */
	public final NumberPath<java.math.BigDecimal> amount =
			createNumber("amount", java.math.BigDecimal.class);

	/** The charged. */
	public final BooleanPath charged = createBoolean("charged");

	/** The code. */
	public final StringPath code = createString("code");

	/** The collection instance. */
	public final QCollectionInstance collectionInstance;

	/** The cufee id. */
	public final NumberPath<Integer> cufeeId = createNumber("cufeeId", Integer.class);

	/** The date insertion. */
	// inherited
	public final DateTimePath<java.util.Date> dateInsertion = _super.dateInsertion;

	/** The date last update. */
	// inherited
	public final DateTimePath<java.util.Date> dateLastUpdate = _super.dateLastUpdate;

	/** The enabled. */
	// inherited
	public final BooleanPath enabled = _super.enabled;

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The label. */
	public final StringPath label = createString("label");

	/** The loan instance. */
	public final QLoanInstance loanInstance;

	/** The setting fee. */
	public final NumberPath<Long> settingFee = createNumber("settingFee", Long.class);

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q charge fees.
	 *
	 * @param variable the variable
	 */
	public QChargeFees(String variable) {

		this(ChargeFees.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new q charge fees.
	 *
	 * @param path the path
	 */
	public QChargeFees(Path<? extends ChargeFees> path) {

		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new q charge fees.
	 *
	 * @param metadata the metadata
	 */
	public QChargeFees(PathMetadata metadata) {

		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new q charge fees.
	 *
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QChargeFees(PathMetadata metadata, PathInits inits) {

		this(ChargeFees.class, metadata, inits);
	}

	/**
	 * Instantiates a new q charge fees.
	 *
	 * @param type the type
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QChargeFees(Class<? extends ChargeFees> type, PathMetadata metadata, PathInits inits) {

		super(type, metadata, inits);
		this.collectionInstance =
				inits.isInitialized("collectionInstance")
						? new QCollectionInstance(forProperty("collectionInstance"),
								inits.get("collectionInstance"))
						: null;
		this.loanInstance = inits.isInitialized("loanInstance")
				? new QLoanInstance(forProperty("loanInstance"), inits.get("loanInstance"))
				: null;
	}

}

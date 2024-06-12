package com.acm.utils.models;

import static com.querydsl.core.types.PathMetadataFactory.forVariable;

import javax.annotation.Generated;

import com.querydsl.core.types.Path;
import com.querydsl.core.types.PathMetadata;
import com.querydsl.core.types.dsl.BooleanPath;
import com.querydsl.core.types.dsl.DateTimePath;
import com.querydsl.core.types.dsl.EntityPathBase;
import com.querydsl.core.types.dsl.ListPath;
import com.querydsl.core.types.dsl.NumberPath;
import com.querydsl.core.types.dsl.PathInits;
import com.querydsl.core.types.dsl.SetPath;
import com.querydsl.core.types.dsl.StringPath;

/**
 * QSupplier is a Querydsl query type for Supplier.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QSupplier extends EntityPathBase<Supplier> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1806451299L;

	/** The Constant supplier. */
	public static final QSupplier supplier = new QSupplier("supplier");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The acronyme. */
	public final StringPath acronyme = createString("acronyme");

	/** The activity. */
	public final NumberPath<Long> activity = createNumber("activity", Long.class);

	/** The activity name. */
	public final StringPath activityName = createString("activityName");

	/** The activity start date. */
	public final DateTimePath<java.util.Date> activityStartDate =
			createDateTime("activityStartDate", java.util.Date.class);

	/** The commercial name. */
	public final StringPath commercialName = createString("commercialName");

	/** The conventions. */
	public final SetPath<Convention, QConvention> conventions =
			this.<Convention, QConvention>createSet("conventions", Convention.class,
					QConvention.class, PathInits.DIRECT2);

	/** The date insertion. */
	// inherited
	public final DateTimePath<java.util.Date> dateInsertion = _super.dateInsertion;

	/** The date last update. */
	// inherited
	public final DateTimePath<java.util.Date> dateLastUpdate = _super.dateLastUpdate;

	/** The email. */
	public final StringPath email = createString("email");

	/** The enabled. */
	// inherited
	public final BooleanPath enabled = _super.enabled;

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The identity. */
	public final StringPath identity = createString("identity");

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The is customer. */
	public final BooleanPath isCustomer = createBoolean("isCustomer");

	/** The legal catalog. */
	public final NumberPath<Long> legalCatalog = createNumber("legalCatalog", Long.class);

	/** The list address. */
	public final ListPath<Address, QAddress> listAddress = this.<Address, QAddress>createList(
			"listAddress", Address.class, QAddress.class, PathInits.DIRECT2);

	/** The name. */
	public final StringPath name = createString("name");

	/** The objectif. */
	public final NumberPath<Long> objectif = createNumber("objectif", Long.class);

	/** The periodicity. */
	public final NumberPath<Long> periodicity = createNumber("periodicity", Long.class);

	/** The register number. */
	public final StringPath registerNumber = createString("registerNumber");

	/** The status. */
	public final StringPath status = createString("status");

	/** The telephone. */
	public final StringPath telephone = createString("telephone");

	/** The telephone 2. */
	public final StringPath telephone2 = createString("telephone2");

	/** The type. */
	public final NumberPath<Long> type = createNumber("type", Long.class);

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/** The web site. */
	public final StringPath webSite = createString("webSite");

	/**
	 * Instantiates a new q supplier.
	 *
	 * @param variable the variable
	 */
	public QSupplier(String variable) {

		super(Supplier.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q supplier.
	 *
	 * @param path the path
	 */
	public QSupplier(Path<? extends Supplier> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q supplier.
	 *
	 * @param metadata the metadata
	 */
	public QSupplier(PathMetadata metadata) {

		super(Supplier.class, metadata);
	}

}

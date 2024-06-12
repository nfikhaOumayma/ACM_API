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
 * QCollectionInstance is a Querydsl query type for CollectionInstance.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QCollectionInstance extends EntityPathBase<CollectionInstance> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1842405674L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant collectionInstance. */
	public static final QCollectionInstance collectionInstance =
			new QCollectionInstance("collectionInstance");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The action user. */
	public final StringPath actionUser = createString("actionUser");

	/** The after date. */
	public final StringPath afterDate = createString("afterDate");

	/** The client. */
	public final StringPath client = createString("client");

	/** The collection. */
	public final QAcmCollection collection;

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

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The id acm collection step. */
	public final NumberPath<Integer> idAcmCollectionStep =
			createNumber("idAcmCollectionStep", Integer.class);

	/** The ihm root. */
	public final StringPath ihmRoot = createString("ihmRoot");

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The libelle. */
	public final StringPath libelle = createString("libelle");

	/** The order etape process. */
	public final NumberPath<Integer> orderEtapeProcess =
			createNumber("orderEtapeProcess", Integer.class);

	/** The start date. */
	public final NumberPath<Integer> startDate = createNumber("startDate", Integer.class);

	/** The statut collection. */
	public final StringPath statutCollection = createString("statutCollection");

	/** The step name. */
	public final StringPath stepName = createString("stepName");

	/** The third parties. */
	public final SetPath<AcmThirdParty, QAcmThirdParty> thirdParties =
			this.<AcmThirdParty, QAcmThirdParty>createSet("thirdParties",
					AcmThirdParty.class, QAcmThirdParty.class,
					PathInits.DIRECT2);

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q collection instance.
	 *
	 * @param variable the variable
	 */
	public QCollectionInstance(String variable) {

		this(CollectionInstance.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new q collection instance.
	 *
	 * @param path the path
	 */
	public QCollectionInstance(Path<? extends CollectionInstance> path) {

		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new q collection instance.
	 *
	 * @param metadata the metadata
	 */
	public QCollectionInstance(PathMetadata metadata) {

		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new q collection instance.
	 *
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QCollectionInstance(PathMetadata metadata, PathInits inits) {

		this(CollectionInstance.class, metadata, inits);
	}

	/**
	 * Instantiates a new q collection instance.
	 *
	 * @param type the type
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QCollectionInstance(Class<? extends CollectionInstance> type, PathMetadata metadata,
			PathInits inits) {

		super(type, metadata, inits);
		this.collection =
				inits.isInitialized("collection") ? new QAcmCollection(forProperty("collection"))
						: null;
	}

}

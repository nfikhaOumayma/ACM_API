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
 * QAcmThirdParty is a Querydsl query type for AcmThirdParty.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QAcmThirdParty extends EntityPathBase<AcmThirdParty> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -847065485L;

	/** The Constant acmThirdParty. */
	public static final QAcmThirdParty acmThirdParty = new QAcmThirdParty("acmThirdParty");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The access branches. */
	public final StringPath accessBranches = createString("accessBranches");

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The address party. */
	public final StringPath addressParty = createString("addressParty");

	/** The branch description. */
	public final StringPath branchDescription = createString("branchDescription");

	/** The branch ID. */
	public final NumberPath<Integer> branchID = createNumber("branchID", Integer.class);

	/** The branch name. */
	public final StringPath branchName = createString("branchName");

	/** The code postal. */
	public final NumberPath<Integer> code_postal = createNumber("code_postal", Integer.class);

	/** The collections instances. */
	public final SetPath<CollectionInstance, QCollectionInstance> collectionsInstances =
			this.<CollectionInstance, QCollectionInstance>createSet("collectionsInstances",
					CollectionInstance.class, QCollectionInstance.class, PathInits.DIRECT2);

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

	/** The first name. */
	public final StringPath firstName = createString("firstName");

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The last name. */
	public final StringPath lastName = createString("lastName");

	/** The numero rne. */
	public final NumberPath<Long> numero_rne = createNumber("numero_rne", Long.class);

	/** The pays. */
	public final StringPath pays = createString("pays");

	/** The phone number. */
	public final StringPath phoneNumber = createString("phoneNumber");

	/** The statut. */
	public final StringPath statut = createString("statut");

	/** The type. */
	public final StringPath type = createString("type");

	/** The type party. */
	public final StringPath typeParty = createString("typeParty");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/** The ville. */
	public final StringPath ville = createString("ville");

	/**
	 * Instantiates a new q acm third party.
	 *
	 * @param variable the variable
	 */
	public QAcmThirdParty(String variable) {

		super(AcmThirdParty.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q acm third party.
	 *
	 * @param path the path
	 */
	public QAcmThirdParty(Path<? extends AcmThirdParty> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q acm third party.
	 *
	 * @param metadata the metadata
	 */
	public QAcmThirdParty(PathMetadata metadata) {

		super(AcmThirdParty.class, metadata);
	}

}

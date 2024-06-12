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
 * QAcmLoanInstanceAcmGroupeApproval is a Querydsl query type for AcmLoanInstanceAcmGroupeApproval.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QAcmLoanInstanceAcmGroupeApproval
		extends EntityPathBase<AcmLoanInstanceAcmGroupeApproval> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -1619124421L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant acmLoanInstanceAcmGroupeApproval. */
	public static final QAcmLoanInstanceAcmGroupeApproval acmLoanInstanceAcmGroupeApproval =
			new QAcmLoanInstanceAcmGroupeApproval("acmLoanInstanceAcmGroupeApproval");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The date insertion. */
	// inherited
	public final DateTimePath<java.util.Date> dateInsertion = _super.dateInsertion;

	/** The date last update. */
	// inherited
	public final DateTimePath<java.util.Date> dateLastUpdate = _super.dateLastUpdate;

	/** The enabled. */
	// inherited
	public final BooleanPath enabled = _super.enabled;

	/** The groupe. */
	public final QGroupe groupe;

	/** The groupe code. */
	public final StringPath groupeCode = createString("groupeCode");

	/** The groupe name. */
	public final StringPath groupeName = createString("groupeName");

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The loan instance. */
	public final QLoanInstance loanInstance;

	/** The owner. */
	public final StringPath owner = createString("owner");

	/** The owner name. */
	public final StringPath ownerName = createString("ownerName");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/** The validation. */
	public final BooleanPath validation = createBoolean("validation");

	/**
	 * Instantiates a new q acm loan instance acm groupe approval.
	 *
	 * @param variable the variable
	 */
	public QAcmLoanInstanceAcmGroupeApproval(String variable) {

		this(AcmLoanInstanceAcmGroupeApproval.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new q acm loan instance acm groupe approval.
	 *
	 * @param path the path
	 */
	public QAcmLoanInstanceAcmGroupeApproval(
			Path<? extends AcmLoanInstanceAcmGroupeApproval> path) {

		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new q acm loan instance acm groupe approval.
	 *
	 * @param metadata the metadata
	 */
	public QAcmLoanInstanceAcmGroupeApproval(PathMetadata metadata) {

		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new q acm loan instance acm groupe approval.
	 *
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QAcmLoanInstanceAcmGroupeApproval(PathMetadata metadata, PathInits inits) {

		this(AcmLoanInstanceAcmGroupeApproval.class, metadata, inits);
	}

	/**
	 * Instantiates a new q acm loan instance acm groupe approval.
	 *
	 * @param type the type
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QAcmLoanInstanceAcmGroupeApproval(Class<? extends AcmLoanInstanceAcmGroupeApproval> type,
			PathMetadata metadata, PathInits inits) {

		super(type, metadata, inits);
		this.groupe = inits.isInitialized("groupe") ? new QGroupe(forProperty("groupe")) : null;
		this.loanInstance = inits.isInitialized("loanInstance")
				? new QLoanInstance(forProperty("loanInstance"), inits.get("loanInstance"))
				: null;
	}

}

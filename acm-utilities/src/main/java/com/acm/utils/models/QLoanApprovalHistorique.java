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
 * QLoanApprovalHistorique is a Querydsl query type for LoanApprovalHistorique.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QLoanApprovalHistorique extends EntityPathBase<LoanApprovalHistorique> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 531953607L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant loanApprovalHistorique. */
	public static final QLoanApprovalHistorique loanApprovalHistorique =
			new QLoanApprovalHistorique("loanApprovalHistorique");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The approval amount. */
	public final NumberPath<Long> approvalAmount = createNumber("approvalAmount", Long.class);

	/** The approval date. */
	public final DateTimePath<java.util.Date> approvalDate =
			createDateTime("approvalDate", java.util.Date.class);

	/** The approval desicion. */
	public final NumberPath<Integer> approvalDesicion =
			createNumber("approvalDesicion", Integer.class);

	/** The approval desicion label. */
	public final StringPath approvalDesicionLabel = createString("approvalDesicionLabel");

	/** The approval level. */
	public final NumberPath<Integer> approvalLevel = createNumber("approvalLevel", Integer.class);

	/** The approval level label. */
	public final StringPath approvalLevelLabel = createString("approvalLevelLabel");

	/** The approval note. */
	public final StringPath approvalNote = createString("approvalNote");

	/** The approved by. */
	public final StringPath approvedBy = createString("approvedBy");

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

	/** The loan. */
	public final QLoan loan;

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q loan approval historique.
	 *
	 * @param variable the variable
	 */
	public QLoanApprovalHistorique(String variable) {

		this(LoanApprovalHistorique.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new q loan approval historique.
	 *
	 * @param path the path
	 */
	public QLoanApprovalHistorique(Path<? extends LoanApprovalHistorique> path) {

		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new q loan approval historique.
	 *
	 * @param metadata the metadata
	 */
	public QLoanApprovalHistorique(PathMetadata metadata) {

		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new q loan approval historique.
	 *
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QLoanApprovalHistorique(PathMetadata metadata, PathInits inits) {

		this(LoanApprovalHistorique.class, metadata, inits);
	}

	/**
	 * Instantiates a new q loan approval historique.
	 *
	 * @param type the type
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QLoanApprovalHistorique(Class<? extends LoanApprovalHistorique> type,
			PathMetadata metadata, PathInits inits) {

		super(type, metadata, inits);
		this.loan = inits.isInitialized("loan") ? new QLoan(forProperty("loan"), inits.get("loan"))
				: null;
	}

}

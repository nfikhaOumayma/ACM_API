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
import com.querydsl.core.types.dsl.StringPath;

/**
 * QLoanParticipants is a Querydsl query type for LoanParticipants.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QLoanParticipants extends EntityPathBase<LoanParticipants> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1999891015L;

	/** The Constant loanParticipants. */
	public static final QLoanParticipants loanParticipants =
			new QLoanParticipants("loanParticipants");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The date debut. */
	public final DateTimePath<java.util.Date> dateDebut =
			createDateTime("dateDebut", java.util.Date.class);

	/** The date fin. */
	public final DateTimePath<java.util.Date> dateFin =
			createDateTime("dateFin", java.util.Date.class);

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

	/** The id loan. */
	public final NumberPath<Long> idLoan = createNumber("idLoan", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/** The username. */
	public final StringPath username = createString("username");

	/**
	 * Instantiates a new q loan participants.
	 *
	 * @param variable the variable
	 */
	public QLoanParticipants(String variable) {

		super(LoanParticipants.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q loan participants.
	 *
	 * @param path the path
	 */
	public QLoanParticipants(Path<? extends LoanParticipants> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q loan participants.
	 *
	 * @param metadata the metadata
	 */
	public QLoanParticipants(PathMetadata metadata) {

		super(LoanParticipants.class, metadata);
	}

}

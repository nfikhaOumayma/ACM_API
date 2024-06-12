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
 * QLoanHistorique is a Querydsl query type for LoanHistorique.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QLoanHistorique extends EntityPathBase<LoanHistorique> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -346962908L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant loanHistorique. */
	public static final QLoanHistorique loanHistorique = new QLoanHistorique("loanHistorique");

	/** The action. */
	public final StringPath action = createString("action");

	/** The category. */
	public final StringPath category = createString("category");

	/** The date update. */
	public final DateTimePath<java.util.Date> dateUpdate =
			createDateTime("dateUpdate", java.util.Date.class);

	/** The description. */
	public final StringPath description = createString("description");

	/** The id loan historique. */
	public final NumberPath<Long> idLoanHistorique = createNumber("idLoanHistorique", Long.class);

	/** The loan. */
	public final QLoan loan;

	/** The technique information. */
	public final BooleanPath techniqueInformation = createBoolean("techniqueInformation");

	/** The updated by. */
	public final StringPath updatedBy = createString("updatedBy");

	/**
	 * Instantiates a new q loan historique.
	 *
	 * @param variable the variable
	 */
	public QLoanHistorique(String variable) {

		this(LoanHistorique.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new q loan historique.
	 *
	 * @param path the path
	 */
	public QLoanHistorique(Path<? extends LoanHistorique> path) {

		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new q loan historique.
	 *
	 * @param metadata the metadata
	 */
	public QLoanHistorique(PathMetadata metadata) {

		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new q loan historique.
	 *
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QLoanHistorique(PathMetadata metadata, PathInits inits) {

		this(LoanHistorique.class, metadata, inits);
	}

	/**
	 * Instantiates a new q loan historique.
	 *
	 * @param type the type
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QLoanHistorique(Class<? extends LoanHistorique> type, PathMetadata metadata,
			PathInits inits) {

		super(type, metadata, inits);
		this.loan = inits.isInitialized("loan") ? new QLoan(forProperty("loan"), inits.get("loan"))
				: null;
	}

}

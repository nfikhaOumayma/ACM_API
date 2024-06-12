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
import com.querydsl.core.types.dsl.ListPath;
import com.querydsl.core.types.dsl.NumberPath;
import com.querydsl.core.types.dsl.PathInits;
import com.querydsl.core.types.dsl.StringPath;

/**
 * QLoanInstance is a Querydsl query type for LoanInstance.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QLoanInstance extends EntityPathBase<LoanInstance> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 503575228L;

	/** The Constant INITS. */
	private static final PathInits INITS = PathInits.DIRECT2;

	/** The Constant loanInstance. */
	public static final QLoanInstance loanInstance = new QLoanInstance("loanInstance");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The action user. */
	public final StringPath actionUser = createString("actionUser");

	/** The approvers. */
	public final ListPath<AcmLoanInstanceAcmGroupeApproval, QAcmLoanInstanceAcmGroupeApproval> approvers =
			this.<AcmLoanInstanceAcmGroupeApproval, QAcmLoanInstanceAcmGroupeApproval>createList(
					"approvers", AcmLoanInstanceAcmGroupeApproval.class,
					QAcmLoanInstanceAcmGroupeApproval.class, PathInits.DIRECT2);

	/** The client. */
	public final StringPath client = createString("client");

	/** The code. */
	public final NumberPath<Integer> code = createNumber("code", Integer.class);

	/** The code statut loan. */
	public final NumberPath<Long> codeStatutLoan = createNumber("codeStatutLoan", Long.class);

	/** The code staut ib. */
	public final NumberPath<Integer> codeStautIb = createNumber("codeStautIb", Integer.class);

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

	/** The ihm root. */
	public final StringPath ihmRoot = createString("ihmRoot");

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The journal entry. */
	public final StringPath journalEntry = createString("journalEntry");

	/** The libelle. */
	public final StringPath libelle = createString("libelle");

	/** The loan. */
	public final QLoan loan;

	/** The order etape process. */
	public final NumberPath<Integer> orderEtapeProcess =
			createNumber("orderEtapeProcess", Integer.class);

	/** The process name. */
	public final StringPath processName = createString("processName");

	/** The show ib. */
	public final BooleanPath showIb = createBoolean("showIb");

	/** The statut loan. */
	public final StringPath statutLoan = createString("statutLoan");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q loan instance.
	 *
	 * @param variable the variable
	 */
	public QLoanInstance(String variable) {

		this(LoanInstance.class, forVariable(variable), INITS);
	}

	/**
	 * Instantiates a new q loan instance.
	 *
	 * @param path the path
	 */
	public QLoanInstance(Path<? extends LoanInstance> path) {

		this(path.getType(), path.getMetadata(), PathInits.getFor(path.getMetadata(), INITS));
	}

	/**
	 * Instantiates a new q loan instance.
	 *
	 * @param metadata the metadata
	 */
	public QLoanInstance(PathMetadata metadata) {

		this(metadata, PathInits.getFor(metadata, INITS));
	}

	/**
	 * Instantiates a new q loan instance.
	 *
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QLoanInstance(PathMetadata metadata, PathInits inits) {

		this(LoanInstance.class, metadata, inits);
	}

	/**
	 * Instantiates a new q loan instance.
	 *
	 * @param type the type
	 * @param metadata the metadata
	 * @param inits the inits
	 */
	public QLoanInstance(Class<? extends LoanInstance> type, PathMetadata metadata,
			PathInits inits) {

		super(type, metadata, inits);
		this.loan = inits.isInitialized("loan") ? new QLoan(forProperty("loan"), inits.get("loan"))
				: null;
	}

}

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
import com.querydsl.core.types.dsl.SetPath;
import com.querydsl.core.types.dsl.StringPath;

/**
 * QGroupe is a Querydsl query type for Groupe.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QGroupe extends EntityPathBase<Groupe> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -1864016867L;

	/** The Constant groupe. */
	public static final QGroupe groupe = new QGroupe("groupe");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The code. */
	public final StringPath code = createString("code");

	/** The collections. */
	public final SetPath<CollectionStep, QCollectionStep> collections =
			this.<CollectionStep, QCollectionStep>createSet("collections", CollectionStep.class,
					QCollectionStep.class, PathInits.DIRECT2);

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

	/** The group loans instance. */
	public final ListPath<AcmLoanInstanceAcmGroupeApproval, QAcmLoanInstanceAcmGroupeApproval> groupLoansInstance =
			this.<AcmLoanInstanceAcmGroupeApproval, QAcmLoanInstanceAcmGroupeApproval>createList(
					"groupLoansInstance", AcmLoanInstanceAcmGroupeApproval.class,
					QAcmLoanInstanceAcmGroupeApproval.class, PathInits.DIRECT2);

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The libelle. */
	public final StringPath libelle = createString("libelle");

	/** The loans participants. */
	public final SetPath<WorkFlowStep, QWorkFlowStep> loansParticipants =
			this.<WorkFlowStep, QWorkFlowStep>createSet("loansParticipants", WorkFlowStep.class,
					QWorkFlowStep.class, PathInits.DIRECT2);

	/** The loans participants approval. */
	public final SetPath<WorkFlowStep, QWorkFlowStep> loansParticipantsApproval =
			this.<WorkFlowStep, QWorkFlowStep>createSet("loansParticipantsApproval",
					WorkFlowStep.class, QWorkFlowStep.class, PathInits.DIRECT2);

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/** The user profile ID extern. */
	public final NumberPath<Long> userProfileIDExtern =
			createNumber("userProfileIDExtern", Long.class);

	/** The users. */
	public final SetPath<User, QUser> users =
			this.<User, QUser>createSet("users", User.class, QUser.class, PathInits.DIRECT2);

	/**
	 * Instantiates a new q groupe.
	 *
	 * @param variable the variable
	 */
	public QGroupe(String variable) {

		super(Groupe.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q groupe.
	 *
	 * @param path the path
	 */
	public QGroupe(Path<? extends Groupe> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q groupe.
	 *
	 * @param metadata the metadata
	 */
	public QGroupe(PathMetadata metadata) {

		super(Groupe.class, metadata);
	}

}

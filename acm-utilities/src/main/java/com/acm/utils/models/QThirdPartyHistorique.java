/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.utils.models;

import static com.querydsl.core.types.PathMetadataFactory.forVariable;

import javax.annotation.Generated;

import com.querydsl.core.types.Path;
import com.querydsl.core.types.PathMetadata;
import com.querydsl.core.types.dsl.ArrayPath;
import com.querydsl.core.types.dsl.BooleanPath;
import com.querydsl.core.types.dsl.DateTimePath;
import com.querydsl.core.types.dsl.EntityPathBase;
import com.querydsl.core.types.dsl.NumberPath;
import com.querydsl.core.types.dsl.StringPath;

/**
 * QThirdPartyHistorique is a Querydsl query type for ThirdPartyHistorique.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QThirdPartyHistorique extends EntityPathBase<ThirdPartyHistorique> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1921997043L;

	/** The Constant thirdPartyHistorique. */
	public static final QThirdPartyHistorique thirdPartyHistorique =
			new QThirdPartyHistorique("thirdPartyHistorique");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The active loans. */
	public final NumberPath<Integer> activeLoans = createNumber("activeLoans", Integer.class);

	/** The aml pourcentage. */
	public final NumberPath<Integer> amlPourcentage = createNumber("amlPourcentage", Integer.class);

	/** The category. */
	public final StringPath category = createString("category");

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

	/** The id customer. */
	public final NumberPath<Long> idCustomer = createNumber("idCustomer", Long.class);

	/** The id customer guarantor. */
	public final NumberPath<Long> idCustomerGuarantor =
			createNumber("idCustomerGuarantor", Long.class);

	/** The identity customer. */
	public final StringPath identityCustomer = createString("identityCustomer");

	/** The identity customer guarantor. */
	public final StringPath identityCustomerGuarantor = createString("identityCustomerGuarantor");

	/** The id loan. */
	public final NumberPath<Long> idLoan = createNumber("idLoan", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The max num days due. */
	public final NumberPath<Integer> maxNumDaysDue = createNumber("maxNumDaysDue", Integer.class);

	/** The query date. */
	public final DateTimePath<java.util.Date> queryDate =
			createDateTime("queryDate", java.util.Date.class);

	/** The report iscore. */
	public final ArrayPath<byte[], Byte> reportIscore = createArray("reportIscore", byte[].class);

	/** The report tag. */
	public final StringPath reportTag = createString("reportTag");

	/** The request value. */
	public final StringPath requestValue = createString("requestValue");

	/** The response value. */
	public final StringPath responseValue = createString("responseValue");

	/** The score. */
	public final NumberPath<Integer> score = createNumber("score", Integer.class);

	/** The status. */
	public final StringPath status = createString("status");

	/** The total approval amt. */
	public final StringPath totalApprovalAmt = createString("totalApprovalAmt");

	/** The total balance amount. */
	public final StringPath totalBalanceAmount = createString("totalBalanceAmount");

	/** The total monthly installment amt. */
	public final StringPath totalMonthlyInstallmentAmt = createString("totalMonthlyInstallmentAmt");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q third party historique.
	 *
	 * @param variable the variable
	 */
	public QThirdPartyHistorique(String variable) {

		super(ThirdPartyHistorique.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q third party historique.
	 *
	 * @param path the path
	 */
	public QThirdPartyHistorique(Path<? extends ThirdPartyHistorique> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q third party historique.
	 *
	 * @param metadata the metadata
	 */
	public QThirdPartyHistorique(PathMetadata metadata) {

		super(ThirdPartyHistorique.class, metadata);
	}

}

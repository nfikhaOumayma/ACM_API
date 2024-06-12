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
import com.querydsl.core.types.dsl.PathInits;
import com.querydsl.core.types.dsl.SetPath;
import com.querydsl.core.types.dsl.StringPath;

/**
 * QCustomer is a Querydsl query type for Customer.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QCustomer extends EntityPathBase<Customer> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = -219035531L;

	/** The Constant customer. */
	public static final QCustomer customer = new QCustomer("customer");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The account portfolio code. */
	public final StringPath accountPortfolioCode = createString("accountPortfolioCode");

	/** The account portfolio description. */
	public final StringPath accountPortfolioDescription =
			createString("accountPortfolioDescription");

	/** The account portfolio ID. */
	public final NumberPath<Long> accountPortfolioID =
			createNumber("accountPortfolioID", Long.class);

	/** The account year end. */
	public final DateTimePath<java.util.Date> accountYearEnd =
			createDateTime("accountYearEnd", java.util.Date.class);

	/** The acm meza cards. */
	public final SetPath<AcmMezaCard, QAcmMezaCard> acmMezaCards =
			this.<AcmMezaCard, QAcmMezaCard>createSet("acmMezaCards", AcmMezaCard.class,
					QAcmMezaCard.class, PathInits.DIRECT2);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The age. */
	public final NumberPath<Long> age = createNumber("age", Long.class);

	/** The alt name. */
	public final StringPath altName = createString("altName");

	/** The beneficial effective. */
	public final StringPath beneficialEffective = createString("beneficialEffective");

	/** The branches description. */
	public final StringPath branchesDescription = createString("branchesDescription");

	/** The branches name. */
	public final StringPath branchesName = createString("branchesName");

	/** The branch id. */
	public final NumberPath<Integer> branchId = createNumber("branchId", Integer.class);

	/** The correspondance name. */
	public final StringPath correspondanceName = createString("correspondanceName");

	/** The customer address. */
	public final StringPath customerAddress = createString("customerAddress");

	/** The customer id extern. */
	public final NumberPath<Long> customerIdExtern = createNumber("customerIdExtern", Long.class);

	/** The customer links relationships. */
	public final SetPath<CustomerLinksRelationship, QCustomerLinksRelationship> customerLinksRelationships =
			this.<CustomerLinksRelationship, QCustomerLinksRelationship>createSet(
					"customerLinksRelationships", CustomerLinksRelationship.class,
					QCustomerLinksRelationship.class, PathInits.DIRECT2);

	/** The customer name. */
	public final StringPath customerName = createString("customerName");

	/** The customer number. */
	public final StringPath customerNumber = createString("customerNumber");

	/** The customer open date. */
	public final DateTimePath<java.util.Date> customerOpenDate =
			createDateTime("customerOpenDate", java.util.Date.class);

	/** The customer type. */
	public final StringPath customerType = createString("customerType");

	/** The date insertion. */
	public final DateTimePath<java.util.Date> dateInsertion = _super.dateInsertion;

	/** The date last update. */
	public final DateTimePath<java.util.Date> dateLastUpdate = _super.dateLastUpdate;

	/** The date of birth. */
	public final DateTimePath<java.util.Date> dateOfBirth =
			createDateTime("dateOfBirth", java.util.Date.class);

	/** The date of birth hijri. */
	public final StringPath dateOfBirthHijri = createString("dateOfBirthHijri");

	/** The email. */
	public final StringPath email = createString("email");

	/** The enable critical data. */
	public final BooleanPath enableCriticalData = createBoolean("enableCriticalData");

	/** The enabled. */
	public final BooleanPath enabled = _super.enabled;

	/** The fax. */
	public final StringPath fax = createString("fax");

	/** The gender. */
	public final StringPath gender = createString("gender");

	/** The ib customer id. */
	public final NumberPath<Long> ibCustomerId = createNumber("ibCustomerId", Long.class);

	/** The ib loans. */
	public final SetPath<IBLoan, QIBLoan> ibLoans = this.<IBLoan, QIBLoan>createSet("ibLoans",
			IBLoan.class, QIBLoan.class, PathInits.DIRECT2);

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The identity. */
	public final StringPath identity = createString("identity");

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The is customer. */
	public final BooleanPath isCustomer = createBoolean("isCustomer");

	/** The is supplier. */
	public final BooleanPath isSupplier = createBoolean("isSupplier");

	/** The loans. */
	public final SetPath<Loan, QLoan> loans =
			this.<Loan, QLoan>createSet("loans", Loan.class, QLoan.class, PathInits.DIRECT2);

	/** The marital status. */
	public final StringPath maritalStatus = createString("maritalStatus");

	/** The meza card status. */
	public final StringPath mezaCardStatus = createString("mezaCardStatus");

	/** The organisation id. */
	public final NumberPath<Long> organisationId = createNumber("organisationId", Long.class);

	/** The organisation id extern. */
	public final NumberPath<Long> organisationIdExtern =
			createNumber("organisationIdExtern", Long.class);

	/** The organization name. */
	public final StringPath organizationName = createString("organizationName");

	/** The person id extern. */
	public final NumberPath<Long> personIdExtern = createNumber("personIdExtern", Long.class);

	/** The photo. */
	public final ArrayPath<byte[], Byte> photo = createArray("photo", byte[].class);

	/** The prospection comment. */
	public final StringPath prospectionComment = createString("prospectionComment");

	/** The prospection source. */
	public final StringPath prospectionSource = createString("prospectionSource");

	/** The register number. */
	public final StringPath registerNumber = createString("registerNumber");

	/** The sector. */
	public final StringPath sector = createString("sector");

	/** The solidarity name. */
	public final StringPath solidarityName = createString("solidarityName");

	/** The supplier recommandation. */
	public final NumberPath<Long> supplierRecommandation =
			createNumber("supplierRecommandation", Long.class);

	/** The telephone 1. */
	public final StringPath telephone1 = createString("telephone1");

	/** The telephone 2. */
	public final StringPath telephone2 = createString("telephone2");

	/** The telephone 3. */
	public final StringPath telephone3 = createString("telephone3");

	/** The update customer. */
	public final BooleanPath updateCustomer = createBoolean("updateCustomer");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/** The web site. */
	public final StringPath webSite = createString("webSite");

	/**
	 * Instantiates a new q customer.
	 *
	 * @param variable the variable
	 */
	public QCustomer(String variable) {

		super(Customer.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q customer.
	 *
	 * @param path the path
	 */
	public QCustomer(Path<? extends Customer> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q customer.
	 *
	 * @param metadata the metadata
	 */
	public QCustomer(PathMetadata metadata) {

		super(Customer.class, metadata);
	}

}

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
 * QProduct is a Querydsl query type for Product.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QProduct extends EntityPathBase<Product> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 1742116216L;

	/** The Constant product. */
	public static final QProduct product = new QProduct("product");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The about product. */
	public final StringPath aboutProduct = createString("aboutProduct");

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The aml check pourcentage. */
	public final NumberPath<java.math.BigDecimal> amlCheckPourcentage =
			createNumber("amlCheckPourcentage", java.math.BigDecimal.class);

	/** The capitalise interest when refinancing. */
	public final BooleanPath capitaliseInterestWhenRefinancing =
			createBoolean("capitaliseInterestWhenRefinancing");

	/** The code. */
	public final StringPath code = createString("code");

	/** The creation date abacus. */
	public final DateTimePath<java.util.Date> creationDateAbacus =
			createDateTime("creationDateAbacus", java.util.Date.class);

	/** The cu insurance ID. */
	public final NumberPath<Integer> cuInsuranceID = createNumber("cuInsuranceID", Integer.class);

	/** The currency. */
	public final StringPath currency = createString("currency");

	/** The date insertion. */
	// inherited
	public final DateTimePath<java.util.Date> dateInsertion = _super.dateInsertion;

	/** The date last update. */
	// inherited
	public final DateTimePath<java.util.Date> dateLastUpdate = _super.dateLastUpdate;

	/** The decimal. */
	public final NumberPath<Integer> decimal = createNumber("decimal", Integer.class);

	/** The description. */
	public final StringPath description = createString("description");

	/** The edit date abacus. */
	public final DateTimePath<java.util.Date> editDateAbacus =
			createDateTime("editDateAbacus", java.util.Date.class);

	/** The enabled. */
	// inherited
	public final BooleanPath enabled = _super.enabled;

	/** The flat interest rate. */
	public final NumberPath<java.math.BigDecimal> flatInterestRate =
			createNumber("flatInterestRate", java.math.BigDecimal.class);

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The insurance vat. */
	public final NumberPath<java.math.BigDecimal> insuranceVat =
			createNumber("insuranceVat", java.math.BigDecimal.class);

	/** The issue fee amount 1. */
	public final NumberPath<java.math.BigDecimal> issueFeeAmount1 =
			createNumber("issueFeeAmount1", java.math.BigDecimal.class);

	/** The issue fee amount 2. */
	public final NumberPath<java.math.BigDecimal> issueFeeAmount2 =
			createNumber("issueFeeAmount2", java.math.BigDecimal.class);

	/** The issue fee percentage 1. */
	public final NumberPath<java.math.BigDecimal> issueFeePercentage1 =
			createNumber("issueFeePercentage1", java.math.BigDecimal.class);

	/** The issue fee percentage 2. */
	public final NumberPath<java.math.BigDecimal> issueFeePercentage2 =
			createNumber("issueFeePercentage2", java.math.BigDecimal.class);

	/** The issue fee percentage 3. */
	public final NumberPath<java.math.BigDecimal> issueFeePercentage3 =
			createNumber("issueFeePercentage3", java.math.BigDecimal.class);

	/** The issue fee percentage 4. */
	public final NumberPath<java.math.BigDecimal> issueFeePercentage4 =
			createNumber("issueFeePercentage4", java.math.BigDecimal.class);

	/** The issue fee VAT 1. */
	public final NumberPath<java.math.BigDecimal> issueFeeVAT1 =
			createNumber("issueFeeVAT1", java.math.BigDecimal.class);

	/** The issue fee VAT 2. */
	public final NumberPath<java.math.BigDecimal> issueFeeVAT2 =
			createNumber("issueFeeVAT2", java.math.BigDecimal.class);

	/** The max accounts. */
	public final NumberPath<Integer> maxAccounts = createNumber("maxAccounts", Integer.class);

	/** The max active loans. */
	public final NumberPath<Integer> maxActiveLoans = createNumber("maxActiveLoans", Integer.class);

	/** The maximum age. */
	public final NumberPath<Integer> maximumAge = createNumber("maximumAge", Integer.class);

	/** The maximum balance. */
	public final NumberPath<java.math.BigDecimal> maximumBalance =
			createNumber("maximumBalance", java.math.BigDecimal.class);

	/** The maximum deferred period. */
	public final NumberPath<Integer> maximumDeferredPeriod =
			createNumber("maximumDeferredPeriod", Integer.class);

	/** The maximum term. */
	public final NumberPath<Integer> maximumTerm = createNumber("maximumTerm", Integer.class);

	/** The max num days due. */
	public final NumberPath<Integer> maxNumDaysDue = createNumber("maxNumDaysDue", Integer.class);

	/** The max num days expiry. */
	public final NumberPath<Integer> maxNumDaysExpiry =
			createNumber("maxNumDaysExpiry", Integer.class);

	/** The max score. */
	public final NumberPath<Integer> maxScore = createNumber("maxScore", Integer.class);

	/** The min active loans. */
	public final NumberPath<Integer> minActiveLoans = createNumber("minActiveLoans", Integer.class);

	/** The minimum age. */
	public final NumberPath<Integer> minimumAge = createNumber("minimumAge", Integer.class);

	/** The minimum deferred period. */
	public final NumberPath<Integer> minimumDeferredPeriod =
			createNumber("minimumDeferredPeriod", Integer.class);

	/** The minimum term. */
	public final NumberPath<Integer> minimumTerm = createNumber("minimumTerm", Integer.class);

	/** The min num days due. */
	public final NumberPath<Integer> minNumDaysDue = createNumber("minNumDaysDue", Integer.class);

	/** The min score. */
	public final NumberPath<Integer> minScore = createNumber("minScore", Integer.class);

	/** The product details. */
	public final SetPath<ProductDetails, QProductDetails> productDetails =
			this.<ProductDetails, QProductDetails>createSet("productDetails", ProductDetails.class,
					QProductDetails.class, PathInits.DIRECT2);

	/** The product grp. */
	public final BooleanPath productGrp = createBoolean("productGrp");

	/** The product id abacus. */
	public final NumberPath<Long> productIdAbacus = createNumber("productIdAbacus", Long.class);

	/** The product indiv. */
	public final BooleanPath productIndiv = createBoolean("productIndiv");

	/** The product org. */
	public final BooleanPath productOrg = createBoolean("productOrg");

	/** The product type abacus. */
	public final NumberPath<Long> productTypeAbacus = createNumber("productTypeAbacus", Long.class);

	/** The rate. */
	public final NumberPath<java.math.BigDecimal> rate =
			createNumber("rate", java.math.BigDecimal.class);

	/** The rate end date. */
	public final DateTimePath<java.util.Date> rateEndDate =
			createDateTime("rateEndDate", java.util.Date.class);

	/** The rate start date. */
	public final DateTimePath<java.util.Date> rateStartDate =
			createDateTime("rateStartDate", java.util.Date.class);

	/** The refinance. */
	public final BooleanPath refinance = createBoolean("refinance");

	/** The renewal percentage. */
	public final NumberPath<java.math.BigDecimal> renewalPercentage =
			createNumber("renewalPercentage", java.math.BigDecimal.class);

	/** The round type. */
	public final StringPath roundType = createString("roundType");

	/** The topup. */
	public final BooleanPath topup = createBoolean("topup");

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/** The use schedule interest. */
	public final BooleanPath useScheduleInterest = createBoolean("useScheduleInterest");

	/**
	 * Instantiates a new q product.
	 *
	 * @param variable the variable
	 */
	public QProduct(String variable) {

		super(Product.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q product.
	 *
	 * @param path the path
	 */
	public QProduct(Path<? extends Product> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q product.
	 *
	 * @param metadata the metadata
	 */
	public QProduct(PathMetadata metadata) {

		super(Product.class, metadata);
	}

}

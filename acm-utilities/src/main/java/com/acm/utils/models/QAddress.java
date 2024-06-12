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
 * QAddress is a Querydsl query type for Address.
 */
@Generated("com.querydsl.codegen.EntitySerializer")
public class QAddress extends EntityPathBase<Address> {

	/** The Constant serialVersionUID. */
	private static final long serialVersionUID = 903898237L;

	/** The Constant address. */
	public static final QAddress address = new QAddress("address");

	/** The super. */
	public final QGenericModel _super = new QGenericModel(this);

	/** The acm version. */
	// inherited
	public final NumberPath<Integer> acmVersion = _super.acmVersion;

	/** The address 1. */
	public final StringPath address1 = createString("address1");

	/** The address 1 id. */
	public final NumberPath<Long> address1Id = createNumber("address1Id", Long.class);

	/** The address 2. */
	public final StringPath address2 = createString("address2");

	/** The address 2 id. */
	public final NumberPath<Long> address2Id = createNumber("address2Id", Long.class);

	/** The address 3. */
	public final StringPath address3 = createString("address3");

	/** The address 3 id. */
	public final NumberPath<Long> address3Id = createNumber("address3Id", Long.class);

	/** The address type id. */
	public final NumberPath<Long> addressTypeId = createNumber("addressTypeId", Long.class);

	/** The country. */
	public final StringPath country = createString("country");

	/** The country id. */
	public final NumberPath<Long> countryId = createNumber("countryId", Long.class);

	/** The county. */
	public final StringPath county = createString("county");

	/** The county id. */
	public final NumberPath<Long> countyId = createNumber("countyId", Long.class);

	/** The customer id. */
	public final NumberPath<Long> customerId = createNumber("customerId", Long.class);

	/** The date insertion. */
	// inherited
	public final DateTimePath<java.util.Date> dateInsertion = _super.dateInsertion;

	/** The date last update. */
	// inherited
	public final DateTimePath<java.util.Date> dateLastUpdate = _super.dateLastUpdate;

	/** The date moved in. */
	public final DateTimePath<java.util.Date> dateMovedIn =
			createDateTime("dateMovedIn", java.util.Date.class);

	/** The date moved out. */
	public final DateTimePath<java.util.Date> dateMovedOut =
			createDateTime("dateMovedOut", java.util.Date.class);

	/** The enabled. */
	// inherited
	public final BooleanPath enabled = _super.enabled;

	/** The id. */
	public final NumberPath<Long> id = createNumber("id", Long.class);

	/** The id address abacus. */
	public final NumberPath<Long> idAddressAbacus = createNumber("idAddressAbacus", Long.class);

	/** The insert by. */
	// inherited
	public final StringPath insertBy = _super.insertBy;

	/** The is primary. */
	public final BooleanPath isPrimary = createBoolean("isPrimary");

	/** The lan. */
	public final StringPath lan = createString("lan");

	/** The lng. */
	public final StringPath lng = createString("lng");

	/** The postal code. */
	public final StringPath postalCode = createString("postalCode");

	/** The postal code id. */
	public final NumberPath<Long> postalCodeId = createNumber("postalCodeId", Long.class);

	/** The region. */
	public final StringPath region = createString("region");

	/** The region id. */
	public final NumberPath<Long> regionId = createNumber("regionId", Long.class);

	/** The state. */
	public final StringPath state = createString("state");

	/** The state id. */
	public final NumberPath<Long> stateId = createNumber("stateId", Long.class);

	/** The town city. */
	public final StringPath townCity = createString("townCity");

	/** The town city id. */
	public final NumberPath<Long> townCityId = createNumber("townCityId", Long.class);

	/** The updated by. */
	// inherited
	public final StringPath updatedBy = _super.updatedBy;

	/**
	 * Instantiates a new q address.
	 *
	 * @param variable the variable
	 */
	public QAddress(String variable) {

		super(Address.class, forVariable(variable));
	}

	/**
	 * Instantiates a new q address.
	 *
	 * @param path the path
	 */
	public QAddress(Path<? extends Address> path) {

		super(path.getType(), path.getMetadata());
	}

	/**
	 * Instantiates a new q address.
	 *
	 * @param metadata the metadata
	 */
	public QAddress(PathMetadata metadata) {

		super(Address.class, metadata);
	}

}

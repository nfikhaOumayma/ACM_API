/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.AddressDTO;

/**
 * {@link AddressDTOAbacusRowMapper} class.
 *
 * @author HaythemBenizid
 * @since 1.0.6
 */
public class AddressDTOAbacusRowMapper implements RowMapper<AddressDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public AddressDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		return new AddressDTO(rs.getLong("CustomerID"), rs.getString("Address1"),
				rs.getString("Address2"), rs.getString("Address3"), rs.getString("TownCity"),
				rs.getString("County"), rs.getString("State"), rs.getString("PostalCode"),
				rs.getString("Country"), rs.getString("Region"), rs.getLong("Address1ID"),
				rs.getLong("Address2ID"), rs.getLong("Address3ID"), rs.getLong("TownCityID"),
				rs.getLong("CountyID"), rs.getLong("StateID"), rs.getLong("PostalCodeID"),
				rs.getLong("CountryID"), rs.getLong("RegionID"), rs.getLong("AddressTypeID"),
				rs.getDate("DateMovedIn"), rs.getDate("DateMovedOut"),
				rs.getBoolean("PrimaryAddressType"), rs.getLong("AddressID"));
	}
}

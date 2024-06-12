/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.AddressTypeDTO;

/**
 * {@link AddressTypeDTOAbacusRowMapper} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public class AddressTypeDTOAbacusRowMapper implements RowMapper<AddressTypeDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public AddressTypeDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		AddressTypeDTO addressTypeDTO = new AddressTypeDTO();
		addressTypeDTO.setAddressTypeID(rs.getInt("AddressTypeID"));
		addressTypeDTO.setName(rs.getString("Name"));
		addressTypeDTO.setPrimaryAddressType(rs.getInt("PrimaryAddressType"));
		return addressTypeDTO;
	}
}

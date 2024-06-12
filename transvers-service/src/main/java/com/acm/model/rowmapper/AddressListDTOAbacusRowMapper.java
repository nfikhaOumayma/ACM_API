/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.AddressListDTO;

/**
 * {@link AddressListDTOAbacusRowMapper} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public class AddressListDTOAbacusRowMapper implements RowMapper<AddressListDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public AddressListDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		AddressListDTO addressListDTO = new AddressListDTO();
		addressListDTO.setAddressListID(rs.getInt("AddressListID"));
		addressListDTO.setName(rs.getString("Name"));
		addressListDTO.setParentAddressListID(rs.getInt("ParentAddressListID"));
		return addressListDTO;
	}
}

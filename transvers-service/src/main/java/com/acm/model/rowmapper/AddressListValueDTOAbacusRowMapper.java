/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.AddressListValueDTO;

/**
 * {@link AddressListValueDTOAbacusRowMapper} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public class AddressListValueDTOAbacusRowMapper implements RowMapper<AddressListValueDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public AddressListValueDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		AddressListValueDTO addressListValueDTO = new AddressListValueDTO();
		addressListValueDTO.setAddressListValueID(rs.getInt("AddressListValueID"));
		addressListValueDTO.setName(rs.getString("Name"));
		addressListValueDTO.setAddressListID(rs.getInt("AddressListID"));
		addressListValueDTO.setParentAddressListValueID(rs.getInt("ParentAddressListValueID"));
		addressListValueDTO.setCode(rs.getString("Code"));
		addressListValueDTO.setcUAccountPortfolioId(rs.getLong("CUAccountPortfolioId"));
		return addressListValueDTO;
	}
}

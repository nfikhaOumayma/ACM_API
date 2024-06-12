/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.model.rowmapper;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.springframework.jdbc.core.RowMapper;

import com.acm.utils.dtos.AddressSettingAbacusDTO;

/**
 * {@link AddressSettingDTOAbacusRowMapper} class.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
public class AddressSettingDTOAbacusRowMapper implements RowMapper<AddressSettingAbacusDTO> {

	/*
	 * (non-Javadoc)
	 * @see org.springframework.jdbc.core.RowMapper#mapRow(java.sql.ResultSet, int)
	 */
	@Override
	public AddressSettingAbacusDTO mapRow(ResultSet rs, int rowNum) throws SQLException {

		AddressSettingAbacusDTO addressSettingsDTO = new AddressSettingAbacusDTO();
		addressSettingsDTO.setAddressField(rs.getString("AddressField"));
		addressSettingsDTO.setLabel(rs.getString("Label"));
		addressSettingsDTO.setDefaultText(rs.getString("DefaultText"));
		addressSettingsDTO.setDefaultAddressListValueID(rs.getInt("DefaultAddressListValueID"));
		addressSettingsDTO.setCustomer(rs.getBoolean("Customer"));
		addressSettingsDTO.setUseList(rs.getBoolean("UseList"));
		addressSettingsDTO.setAddressListID(rs.getInt("AddressListID"));
		addressSettingsDTO.setRequired(rs.getBoolean("Required"));
		return addressSettingsDTO;
	}
}

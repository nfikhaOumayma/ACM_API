/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.configurationprocessor.json.JSONObject;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.constants.common.CommonFunctions;
import com.acm.service.AddressSettingService;
import com.acm.utils.dtos.AddressListDTO;
import com.acm.utils.dtos.AddressListValueDTO;
import com.acm.utils.dtos.AddressSettingAbacusDTO;
import com.acm.utils.dtos.AddressSettingDTO;
import com.acm.utils.dtos.AddressTypeDTO;
import com.acm.utils.enums.SettingAddressTable;

/**
 * This class @{link AddressSettingController} used to control all the {@link AddressSettingDTO}
 * requests.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
@RestController
@RequestMapping("/address-settings")
public class AddressSettingController {

	/** The AddressSetting service. */
	@Autowired
	private AddressSettingService addressSettingService;

	/**
	 * Find {@link List} of {@link AddressSettingDTO} by Requested params.
	 *
	 * @author HaythemBenizid
	 * @param addressSettingDTO the addressSetting DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<AddressSettingDTO> find(@RequestBody AddressSettingDTO addressSettingDTO) {

		return addressSettingService.find(addressSettingDTO);
	}

	/**
	 * Find address type.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/find-address-type")
	public List<AddressTypeDTO> findAddressType() {

		List<AddressTypeDTO> addressTypeDTOs = new ArrayList<>();
		List<AddressSettingDTO> addressSettingDTOs =
				addressSettingService.find(new AddressSettingDTO(null, null,
						SettingAddressTable.ADDRESS_TYPE.tableName(), null));
		addressSettingDTOs.forEach(addressSettingDTO -> addressTypeDTOs
				.add((AddressTypeDTO) CommonFunctions.convertJSONStringtoObject(
						addressSettingDTO.getValueJson(), AddressTypeDTO.class)));
		return addressTypeDTOs;
	}

	/**
	 * Find settings address.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("find-settings-address")
	public List<AddressSettingAbacusDTO> findSettingsAddress() {

		List<AddressSettingAbacusDTO> addressSettingAbacusDTOs = new ArrayList<>();
		List<AddressSettingDTO> addressSettingDTOs =
				addressSettingService.find(new AddressSettingDTO(null, null,
						SettingAddressTable.SETTINGS_ADDRESS.tableName(), null));
		addressSettingDTOs.forEach(addressSettingDTO -> addressSettingAbacusDTOs
				.add((AddressSettingAbacusDTO) CommonFunctions.convertJSONStringtoObject(
						addressSettingDTO.getValueJson(), AddressSettingAbacusDTO.class)));
		return addressSettingAbacusDTOs;
	}

	/**
	 * Find address list by given IDs.
	 *
	 * @author HaythemBenizid
	 * @param idAddressList the id address list
	 * @return the list
	 */
	@PostMapping("/find-address-list")
	public List<AddressListDTO> findAddressList(@RequestBody List<String> idAddressList) {

		List<AddressListDTO> addressListDTOs = new ArrayList<>();
		AddressSettingDTO addressSettingDTO = new AddressSettingDTO(null, null,
				SettingAddressTable.ADDRESS_LIST.tableName(), null);
		addressSettingDTO.setIdAddressList(idAddressList);
		List<AddressSettingDTO> addressSettingDTOs = addressSettingService.find(addressSettingDTO);
		addressSettingDTOs.forEach(addressSetting -> addressListDTOs
				.add((AddressListDTO) CommonFunctions.convertJSONStringtoObject(
						addressSetting.getValueJson(), AddressListDTO.class)));
		return addressListDTOs;
	}

	/**
	 * Find address list value.
	 *
	 * @author HaythemBenizid
	 * @param addressSettingDTO the address setting DTO
	 * @return the list
	 */
	@PostMapping("/find-address-list-value")
	public List<AddressListValueDTO> findAddressListValue(
			@RequestBody AddressSettingDTO addressSettingDTO) {

		List<AddressListValueDTO> addressListValueDTOs = new ArrayList<>();
		addressSettingDTO.setTableAbacusName(SettingAddressTable.ADDRESS_LIST_VALUE.tableName());
		List<AddressSettingDTO> addressSettingDTOs = addressSettingService.find(addressSettingDTO);
		addressSettingDTOs.forEach(addressSetting -> addressListValueDTOs
				.add((AddressListValueDTO) CommonFunctions.convertJSONStringtoObject(
						addressSetting.getValueJson(), AddressListValueDTO.class)));
		return addressListValueDTOs;
	}

	/**
	 * Load setting from ABACUS (USED ONLY BY BATCH).
	 * 
	 * @author HaythemBenizid
	 */
	@GetMapping("/load-setting")
	public void loadSettingFromAbacus() {

		addressSettingService.loadSettingFromAbacus();
	}

	/**
	 * Reset setting address from ABACUS DB.
	 *
	 * @author HaythemBenizid
	 * @return the response entity OK
	 */
	@GetMapping("/reset-setting")
	public String resetSettingFromAbacus() {

		addressSettingService.resetSettingFromAbacus();
		return JSONObject.quote("Reset Address DONE !!");
	}
}

/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.service.AddressSettingAbacusService;
import com.acm.utils.dtos.AddressDTO;
import com.acm.utils.dtos.AddressListDTO;
import com.acm.utils.dtos.AddressListValueDTO;
import com.acm.utils.dtos.AddressSettingAbacusDTO;
import com.acm.utils.dtos.AddressTypeDTO;

/**
 * This class @{link LoadDataAddressController}.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
@RestController
@RequestMapping("/load-data-abacus")
public class LoadDataAddressController {

	/** The setting address abacus service. */
	@Autowired
	private AddressSettingAbacusService addressSettingAbacusService;

	/**
	 * Find address list.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/address/find-address-list")
	public List<AddressListDTO> findAddressList() {

		return addressSettingAbacusService.findAddressList();
	}

	/**
	 * Find address list value.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/address/find-address-list-value")
	public List<AddressListValueDTO> findAddressListValue() {

		return addressSettingAbacusService.findAddressListValue();
	}

	/**
	 * Find address type.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/address/find-address-type")
	public List<AddressTypeDTO> findAddressType() {

		return addressSettingAbacusService.findAddressType();
	}

	/**
	 * Find settings address.
	 * 
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/address/find-settings-address")
	public List<AddressSettingAbacusDTO> findSettingsAddress() {

		return addressSettingAbacusService.findSettingsAddress();
	}

	/**
	 * Load address for customer.
	 *
	 * @author HaythemBenizid
	 * @param limite the limite
	 * @return the list
	 */
	@GetMapping("/address/load-for-customer/{limite}")
	public List<AddressDTO> loadAddressForCustomer(@PathVariable("limite") Long limite) {

		return addressSettingAbacusService.loadAddressForCustomer(limite);
	}

	/**
	 * Load address by customer.
	 *
	 * @author HaythemBenizid
	 * @param idCustomer the id customer
	 * @return the list
	 */
	@GetMapping("/address/load-by-customer/{idCustomer}")
	public List<AddressDTO> loadAddressByCustomer(@PathVariable("idCustomer") Long idCustomer) {

		return addressSettingAbacusService.loadAddressByCustomer(idCustomer);
	}
}

/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.configurationprocessor.json.JSONObject;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.UserDefinedFieldGroupService;
import com.acm.service.UserDefinedFieldListValuesService;
import com.acm.service.UserDefinedFieldsService;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.dtos.UserDefinedFieldGroupDTO;
import com.acm.utils.dtos.UserDefinedFieldListValuesDTO;
import com.acm.utils.dtos.UserDefinedFieldsDTO;

/**
 * This class @{link UDFSettingController}.
 *
 * @author HaythemBenizid
 * @since 1.0.5
 */
@RestController
@RequestMapping("/udf-settings")
public class UDFSettingController {

	/** The user defined field group service. */
	@Autowired
	private UserDefinedFieldGroupService userDefinedFieldGroupService;

	/** The user defined field list values service. */
	@Autowired
	private UserDefinedFieldListValuesService userDefinedFieldListValuesService;

	/** The user defined fields service. */
	@Autowired
	private UserDefinedFieldsService userDefinedFieldsService;

	/**
	 * Find a list of UserDefinedFieldGroupDTO objects based on the provided criteria.
	 * 
	 * @author HaythemBenizid
	 * @param userDefinedFieldGroupDTO The criteria for filtering UserDefinedFieldGroupDTO objects.
	 *        returned.
	 * @return A list of UserDefinedFieldGroupDTO objects that match the criteria.
	 */
	@PostMapping("/udf-group-setting")
	public List<UserDefinedFieldGroupDTO> find(
			@RequestBody UserDefinedFieldGroupDTO userDefinedFieldGroupDTO) {

		return userDefinedFieldGroupService.find(userDefinedFieldGroupDTO);
	}

	/**
	 * Saves a UserDefinedFieldGroupDTO.
	 *
	 * @author nrmila
	 * @param userDefinedFieldGroupDTO The UserDefinedFieldGroupDTO to be saved.
	 * @return The saved UserDefinedFieldGroupDTO.
	 */
	@PostMapping("/udf-group-setting/save")
	public UserDefinedFieldGroupDTO saveUserDefinedFieldGroup(
			@RequestBody UserDefinedFieldGroupDTO userDefinedFieldGroupDTO) {

		return userDefinedFieldGroupService.save(userDefinedFieldGroupDTO, new UserDTO());
	}

	/**
	 * Find {@link List} of {@link UserDefinedFieldsDTO} by Requested params.
	 *
	 * @author HaythemBenizid
	 * @param userDefinedFieldsDTO the user defined fields DTO
	 * @return the list
	 */
	@PostMapping("/udf-fields-setting")
	public List<UserDefinedFieldsDTO> find(@RequestBody UserDefinedFieldsDTO userDefinedFieldsDTO) {

		return userDefinedFieldsService.find(userDefinedFieldsDTO);
	}

	/**
	 * Saves UserDefinedFieldsDTO.
	 *
	 * @author nrmila
	 * @param userDefinedFieldsDTO The UserDefinedFieldsDTO to be saved.
	 * @return The saved UserDefinedFieldsDTO.
	 */
	@PostMapping("/udf-fields-setting/save")
	public UserDefinedFieldsDTO saveUserDefinedFieldsDTO(
			@RequestBody UserDefinedFieldsDTO userDefinedFieldsDTO) {

		return userDefinedFieldsService.save(userDefinedFieldsDTO, new UserDTO());
	}

	/**
	 * Find UDF field by list ids.
	 * 
	 * @author idridi
	 * @param userDefinedFieldsDTOs the user defined fields DT os
	 * @return the list
	 */
	@PostMapping("/find-udf-fields-by-idAbacus")
	public List<UserDefinedFieldsDTO> findUDFFieldByListIds(
			@RequestBody List<UserDefinedFieldsDTO> userDefinedFieldsDTOs) {

		return userDefinedFieldsService.findUDFFieldByListIds(userDefinedFieldsDTOs);
	}

	/**
	 * Find UDF field by ids.
	 *
	 * @param ids the ids
	 * @return the list
	 */
	@PostMapping("/find-udf-fields-by-id")
	public List<UserDefinedFieldsDTO> findUDFFieldByIds(@RequestBody List<Long> ids) {

		return userDefinedFieldsService.findUDFFieldByIds(ids);
	}

	/**
	 * Find {@link List} of {@link UserDefinedFieldListValuesDTO} by given params (idUDFListLink is
	 * REQUIRED).
	 *
	 * @author HaythemBenizid
	 * @param userDefinedFieldListValuesDTO the user defined field list values DTO
	 * @return the list
	 */
	@PostMapping("/udf-list-value")
	public List<UserDefinedFieldListValuesDTO> find(
			@RequestBody UserDefinedFieldListValuesDTO userDefinedFieldListValuesDTO) {

		return userDefinedFieldListValuesService.find(userDefinedFieldListValuesDTO);
	}

	/**
	 * Saves a list of User Defined Field list values. This method accepts a list of
	 * UserDefinedFieldListValuesDTO objects representing the user-defined field list values to be
	 * saved. These values are associated with a specific UserDTO, which is used to determine
	 * ownership or context for these values.
	 *
	 * @author nrmila
	 * @param userDefinedFieldListValuesDTOs A list of UserDefinedFieldListValuesDTO objects
	 *        containing the user-defined field list values to be saved.
	 * @return A list of UserDefinedFieldListValuesDTO objects representing the saved user-defined
	 *         field list values, including any modifications or updates made during the saving
	 *         process.
	 */
	@PostMapping("/udf-list-value/save")
	public List<UserDefinedFieldListValuesDTO> saveUserDefinedFieldListValues(
			@RequestBody List<UserDefinedFieldListValuesDTO> userDefinedFieldListValuesDTOs) {

		return userDefinedFieldListValuesService.saveAll(userDefinedFieldListValuesDTOs,
				new UserDTO());
	}

	/**
	 * Load setting UDF group from abacus (USED ONLY BY BATCH).
	 *
	 * @author HaythemBenizid
	 * @param userDTO the user DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@GetMapping("/load-group-setting")
	public void loadSettingUDFGroupFromAbacus(UserDTO userDTO) throws ResourcesNotFoundException {

		userDefinedFieldGroupService.loadSettingFromAbacus(userDTO);
	}

	/**
	 * Load setting UDF fields from abacus (USED ONLY BY BATCH).
	 *
	 * @author HaythemBenizid
	 * @param userDTO the user DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@GetMapping("/load-fields-setting")
	public void loadSettingUDFFieldsFromAbacus(UserDTO userDTO) throws ResourcesNotFoundException {

		userDefinedFieldsService.loadSettingFromAbacus(userDTO);
	}

	/**
	 * Load setting UDF list value from abacus (USED ONLY BY BATCH).
	 *
	 * @author HaythemBenizid
	 * @param userDTO the user DTO
	 */
	@GetMapping("/load-list-value-setting")
	public void loadSettingUDFListValueFromAbacus(UserDTO userDTO) {

		userDefinedFieldListValuesService.loadSettingFromAbacus(userDTO);
	}

	/**
	 * Reset setting address from ABACUS DB : load-list-value-setting AND load-fields-setting AND
	 * load-group-setting.
	 *
	 * @author HaythemBenizid
	 * @return the response entity OK
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@GetMapping("/reset-setting")
	public String resetSettingFromAbacus() throws ResourcesNotFoundException {

		userDefinedFieldListValuesService.resetSettingFromAbacus();
		return JSONObject.quote("Reset UDF DONE !!");
	}

	/**
	 * Find by id UDF list value.
	 * 
	 * @author yesser somai
	 * @param idUDFListValue the id UDF list value
	 * @return the list
	 */
	@GetMapping("/find-by-idUDF-list-value/{idUDFListValue}")
	public List<UserDefinedFieldListValuesDTO> findByIdUDFListValue(
			@PathVariable("idUDFListValue") Long idUDFListValue) {

		return userDefinedFieldListValuesService.findByIdUDFListValue(idUDFListValue);
	}

}

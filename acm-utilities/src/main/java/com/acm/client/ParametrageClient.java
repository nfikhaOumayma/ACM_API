/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.client;

import java.util.List;

import org.springframework.cloud.netflix.ribbon.RibbonClient;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;

import com.acm.configuration.feignclient.ClientConfiguration;
import com.acm.configuration.feignclient.LoadbalancerRuleFeignConfiguration;
import com.acm.utils.dtos.AMLDataDTO;
import com.acm.utils.dtos.AcmDamagedDataDTO;
import com.acm.utils.dtos.AcmEnvironnementDTO;
import com.acm.utils.dtos.AcmIhmFieldDTO;
import com.acm.utils.dtos.AcmIhmFormDTO;
import com.acm.utils.dtos.AcmTemplateSMSDTO;
import com.acm.utils.dtos.ApplicationFeeDTO;
import com.acm.utils.dtos.ClaimNoteDTO;
import com.acm.utils.dtos.CollectionStepDTO;
import com.acm.utils.dtos.GroupeDTO;
import com.acm.utils.dtos.HabilitationDTO;
import com.acm.utils.dtos.HabilitationIHMRouteDTO;
import com.acm.utils.dtos.MessageDetailsDTO;
import com.acm.utils.dtos.NotificationsDTO;
import com.acm.utils.dtos.ProductDTO;
import com.acm.utils.dtos.RelationshipDTO;
import com.acm.utils.dtos.SettingDocumentProductDTO;
import com.acm.utils.dtos.SettingDocumentTypeDTO;
import com.acm.utils.dtos.SettingGurantorCollateralDTO;
import com.acm.utils.dtos.SettingLevelProcessDTO;
import com.acm.utils.dtos.SettingNotificationsDTO;
import com.acm.utils.dtos.SettingRequiredStepDTO;
import com.acm.utils.dtos.SettingStatutWorkflowDTO;
import com.acm.utils.dtos.UserDefinedFieldGroupDTO;
import com.acm.utils.dtos.UserDefinedFieldListValuesDTO;
import com.acm.utils.dtos.UserDefinedFieldsDTO;
import com.acm.utils.dtos.WorkFlowStepDTO;

// TODO: Auto-generated Javadoc
/**
 * The {@link ParametrageClient} Interface. to inject in order to consume services from
 * parametrage-service
 *
 * @author HaythemBenizid
 * @since 0.8.0
 */
@FeignClient(value = "parametrage-service", configuration = ClientConfiguration.class,
		decode404 = true)
@RibbonClient(name = "parametrage-service",
		configuration = LoadbalancerRuleFeignConfiguration.class)
public interface ParametrageClient {

	/**
	 * Find Setting Document Type by given params.
	 * 
	 * @author HaythemBenizid
	 * @param settingDocumentTypeDTO the setting document type DTO
	 * @return the list
	 */
	@PostMapping("/setting-document-types/")
	List<SettingDocumentTypeDTO> find(@RequestBody SettingDocumentTypeDTO settingDocumentTypeDTO);

	/**
	 * Find Setting Document Product by given params.
	 * 
	 * @author HaythemBenizid
	 * @param settingDocumentProductDTO the setting document product DTO
	 * @return the list
	 */
	@PostMapping("/setting-document-products/")
	List<SettingDocumentProductDTO> find(
			@RequestBody SettingDocumentProductDTO settingDocumentProductDTO);

	/**
	 * Find Setting Gurantor Collateral by given params.
	 *
	 * @author HaythemBenizid
	 * @param settingGurantorCollateralDTO the setting gurantor collateral DTO
	 * @return the list
	 */
	@PostMapping("/setting-gurantor-collateral/")
	List<SettingGurantorCollateralDTO> find(
			@RequestBody SettingGurantorCollateralDTO settingGurantorCollateralDTO);

	/**
	 * Find groupe by given params.
	 *
	 * @author HaythemBenizid
	 * @param groupeDTO the groupe DTO
	 * @return the list
	 */
	@PostMapping("/groupes/")
	List<GroupeDTO> find(@RequestBody GroupeDTO groupeDTO);

	/**
	 * find the habilitations for the CONNECTED USER {@link HabilitationDTO}.
	 *
	 * @author HaythemBenizid
	 * @return the list
	 */
	@GetMapping("/habilitations/")
	List<HabilitationDTO> find();

	/**
	 * Find IHM route by Client (default ACM).
	 * 
	 * @author HaythemBenizid
	 * @param habilitationIHMRouteDTO the habilitation IHM route DTO
	 * @return the list
	 */
	@PostMapping("/habilitations/find-ihm-route")
	List<HabilitationIHMRouteDTO> find(
			@RequestBody HabilitationIHMRouteDTO habilitationIHMRouteDTO);

	/**
	 * Find setting by given params : (product ID & amount are required).
	 *
	 * @author HaythemBenizid
	 * @param settingLevelProcessDTO the setting level process DTO
	 * @return the list
	 */
	@PostMapping("/setting-level-process/")
	List<SettingLevelProcessDTO> find(@RequestBody SettingLevelProcessDTO settingLevelProcessDTO);

	/**
	 * Check athorisation connected user.
	 *
	 * @param acmEnvironnementKey the acm environnement key
	 * @return the boolean
	 */
	@GetMapping("/acm-environnements/check-authorisation-connected-user/{acmEnvironnementKey}")
	Boolean checkAthorisationConnectedUser(
			@PathVariable("acmEnvironnementKey") String acmEnvironnementKey);

	/**
	 * Find parametres by given params.
	 * 
	 * @author HaythemBenizid
	 * @param acmEnvironnementDTO the acm environnement DTO
	 * @return the list
	 */
	@PostMapping("/acm-environnements/")
	List<AcmEnvironnementDTO> find(@RequestBody AcmEnvironnementDTO acmEnvironnementDTO);

	/**
	 * Find by key.
	 *
	 * @author HaythemBenizid
	 * @param key the key
	 * @return the acmEnvironnement DTO
	 */
	@GetMapping("/acm-environnements/{key}")
	AcmEnvironnementDTO find(@PathVariable("key") String key);

	/**
	 * Find by LIKE given KEY.
	 * 
	 * @author HaythemBenizid
	 * @param acmEnvironnementDTO the acmEnvironnement DTO
	 * @return the list
	 */
	@PostMapping("/acm-environnements/find-like-key")
	List<AcmEnvironnementDTO> findLikeKey(@RequestBody AcmEnvironnementDTO acmEnvironnementDTO);

	/**
	 * Find like key with token.
	 *
	 * @param acmEnvironnementDTO the acm environnement DTO
	 * @param token the token
	 * @return the list
	 */
	@PostMapping("/acm-environnements/find-like-key-with-token")
	List<AcmEnvironnementDTO> findLikeKeyWithToken(
			@RequestBody AcmEnvironnementDTO acmEnvironnementDTO,
			@RequestHeader("Authorization") String token);

	/**
	 * update Limite by given key.
	 *
	 * @author HaythemBenizid
	 * @param key the key
	 * @param limite the limite
	 * @return the acm environnement DTO
	 */
	@GetMapping("/acm-environnements/updateLimite/{key}/{limite}")
	AcmEnvironnementDTO updateLimite(@PathVariable("key") String key,
			@PathVariable("limite") String limite);

	/**
	 * Find list SettingStatutWorkflow by given CLIENT (USED ONLY while saving new loan).
	 *
	 * @author HaythemBenizid
	 * @param settingStatutWorkflowDTO the setting statut workflow DTO
	 * @return the list
	 */
	@PostMapping("/setting-statut-workflows/find-loan-process")
	List<SettingStatutWorkflowDTO> findLoanProcess(
			@RequestBody SettingStatutWorkflowDTO settingStatutWorkflowDTO);

	/**
	 * Find list SettingRequiredStep by given params.
	 * 
	 * @author HaythemBenizid
	 * @param settingRequiredStepDTO the setting required step DTO
	 * @return the list
	 */
	@PostMapping("/setting-required-step/")
	List<SettingRequiredStepDTO> find(@RequestBody SettingRequiredStepDTO settingRequiredStepDTO);

	/**
	 * Find {@link List} of {@link UserDefinedFieldsDTO} by Requested params.
	 *
	 * @author HaythemBenizid
	 * @param userDefinedFieldsDTO the user defined fields DTO
	 * @return the list
	 */
	@PostMapping("/udf-settings/udf-fields-setting")
	List<UserDefinedFieldsDTO> find(@RequestBody UserDefinedFieldsDTO userDefinedFieldsDTO);

	/**
	 * Find UDF field by list of ABACUS ids.
	 * 
	 * @author idridi
	 * @param userDefinedFieldsDTOs the user defined fields DT os
	 * @return the list
	 */
	@PostMapping("/udf-settings/find-udf-fields-by-idAbacus")
	List<UserDefinedFieldsDTO> findUDFFieldByListIds(
			@RequestBody List<UserDefinedFieldsDTO> userDefinedFieldsDTOs);

	/**
	 * Find UDF field by udf_field ids in ACM.
	 *
	 * @param ids the ids
	 * @return the list
	 */
	@PostMapping("/udf-settings/find-udf-fields-by-id")
	List<UserDefinedFieldsDTO> findUDFFieldByIds(@RequestBody List<Long> ids);

	/**
	 * Find {@link List} of {@link UserDefinedFieldGroupDTO} by Requested params.
	 *
	 * @author HaythemBenizid
	 * @param userDefinedFieldGroupDTO the user defined field group DTO
	 * @return the list
	 */
	@PostMapping("/udf-settings/udf-group-setting")
	List<UserDefinedFieldGroupDTO> find(
			@RequestBody UserDefinedFieldGroupDTO userDefinedFieldGroupDTO);

	/**
	 * Find Product by id.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @return the product DTO
	 */
	@GetMapping("/products/{id}")
	ProductDTO findProductById(@PathVariable("id") Long id);

	/**
	 * Find product by id.
	 *
	 * @param id the id
	 * @param token the token
	 * @return the product DTO
	 */
	@GetMapping("/products/{id}")
	ProductDTO findProductById(@PathVariable("id") Long id,
			@RequestHeader("Authorization") String token);

	/**
	 * Find {@link List} of {@link ProductDTO} by Requested params.
	 *
	 * @author HaythemBenizid
	 * @param productDTO the product DTO
	 * @return the list
	 */
	@PostMapping("/products/")
	List<ProductDTO> findProducts(@RequestBody ProductDTO productDTO);

	/**
	 * Find {@link List} of {@link UserDefinedFieldListValuesDTO} by Requested params.
	 *
	 * @author MoezMhiri
	 * @param userDefinedFieldListValuesDTO the user defined fields list values DTO
	 * @return the list
	 */
	@PostMapping("/udf-settings/udf-list-value")
	List<UserDefinedFieldListValuesDTO> find(
			@RequestBody UserDefinedFieldListValuesDTO userDefinedFieldListValuesDTO);

	/**
	 * Find by id UDF list value.
	 * 
	 * @author yesser somai
	 * @param idUDFListValue the id UDF list value
	 * @return the list
	 */
	@GetMapping("/udf-settings/find-by-idUDF-list-value/{idUDFListValue}")
	List<UserDefinedFieldListValuesDTO> findByIdUDFListValue(
			@PathVariable("idUDFListValue") Long idUDFListValue);

	/**
	 * Find {@link List} of {@link relationshipDTO}..
	 *
	 * @author MoezMhiri
	 * @return the list
	 */
	@GetMapping("/settings-list-values/find-relationships")
	List<RelationshipDTO> findRelationship();

	/**
	 * Send sms.
	 *
	 * @param messageDetailsDTO the message details DTO
	 * @return the string
	 */
	@PostMapping("/sms-sender/send-sms")
	String sendSms(@RequestBody MessageDetailsDTO messageDetailsDTO);

	/**
	 * Save SMS.
	 *
	 * @param messageDetailsDTO the message details DTO
	 * @return the message details DTO
	 */
	@PostMapping("/sms-sender/save-sms")
	MessageDetailsDTO saveSMS(@RequestBody MessageDetailsDTO messageDetailsDTO);

	/**
	 * Findbycode.
	 *
	 * @param acmTemplateSMSDTO the acm template SMSDTO
	 * @return the acm template SMSDTO
	 */
	@PostMapping("/settings-sms/findTemplate")
	AcmTemplateSMSDTO findbycode(@RequestBody AcmTemplateSMSDTO acmTemplateSMSDTO);

	/**
	 * check-aml {@link List} of {@link AMLDataDTO} by Name.
	 *
	 * @author HaythemBenizid
	 * @param aMLDataDTO the aMLData DTO
	 * @return the list
	 */
	@PostMapping("/aml-settings/check-aml")
	List<AMLDataDTO> findAMLData(@RequestBody AMLDataDTO aMLDataDTO);

	/**
	 * Find IHM route by given params.
	 *
	 * @author HaythemBenizid
	 * @param acmIhmFormDTO the acm ihm form DTO
	 * @return the list
	 */
	@PostMapping("/acm-ihm-form/")
	List<AcmIhmFormDTO> findIhmForm(@RequestBody AcmIhmFormDTO acmIhmFormDTO);

	/**
	 * Find IHM Field by given params.
	 *
	 * @author HaythemBenizid
	 * @param acmIhmFieldDTO the acm ihm field DTO
	 * @return the list
	 */
	@PostMapping("/acm-ihm-field/")
	List<AcmIhmFieldDTO> findIhmField(@RequestBody AcmIhmFieldDTO acmIhmFieldDTO);

	/**
	 * Find groupe by code.
	 * 
	 * @author InesDridi
	 * @param groupeCode the groupe code
	 * @return the groupe DTO
	 */
	@PostMapping("/groupes/find-groupe-by-code")
	GroupeDTO findGroupeByCode(@RequestBody String groupeCode);

	/**
	 * Update acm environnement DTO.
	 * 
	 * @author idridi
	 * @param acmEnvironnementDTO the acm environnement DTO
	 * @return the acm environnement DTO
	 */
	@PutMapping("/acm-environnements/update")
	AcmEnvironnementDTO updateAcmEnvironnementDTO(
			@RequestBody AcmEnvironnementDTO acmEnvironnementDTO);

	/**
	 * Find and add if not exist.
	 *
	 * @author ManelLamloum
	 * @param login the login
	 * @param mezaCardNumber the meza card number
	 * @return the boolean
	 */
	@GetMapping("/in-use-meza-cards/find-and-add-if-not-exist/{mezaCardNumber}/{login}")
	Boolean findAndAddIfNotExist(@PathVariable("login") String login,
			@PathVariable("mezaCardNumber") String mezaCardNumber);

	/**
	 * Delete in use meza card.
	 *
	 * @author ManelLamloum
	 * @param login the login
	 */
	@DeleteMapping("/delete/{login}")
	void deleteInUseMezaCard(@PathVariable("login") String login);

	/**
	 * Send notification via web socket.
	 *
	 * @author mlamloum
	 * @param username the username
	 * @param notificationsDTO the notifications DTO
	 */
	@PostMapping("/setting-notifications/send-notification-via-websocket/{username}")
	void sendNotificationViaWebSocket(@PathVariable("username") String username,
			@RequestBody NotificationsDTO notificationsDTO);

	/**
	 * Save acm damaged data.
	 * 
	 * @author mlamloum
	 * @param acmDamagedDataDTO the acm damaged data DTO
	 * @return the string
	 */
	@PostMapping("/acm-damaged-data/create")
	AcmDamagedDataDTO saveAcmDamagedData(@RequestBody AcmDamagedDataDTO acmDamagedDataDTO);

	/**
	 * Find setting topup refinance by ids.
	 * 
	 * @author idridi
	 * @param ids the ids
	 * @return the product DTO
	 */
	@PostMapping("/products/find-by-ids")
	List<ProductDTO> findByIds(@RequestBody List<Long> ids);

	/**
	 * Find work flow steps.
	 *
	 * @author Yesser somai
	 * @param workFlowStepDTO the work flow step DTO
	 * @return the list of WF steps
	 */
	@PostMapping("/setting-workflow/find")
	List<WorkFlowStepDTO> findWorkFlowSteps(@RequestBody WorkFlowStepDTO workFlowStepDTO);

	/**
	 * Find setting collection.
	 * 
	 * @author idridi
	 * @param collectionStepDTO the collection step DTO
	 * @return the list
	 */
	@PostMapping("/setting-workflow/find-setting-collection")
	List<CollectionStepDTO> findSettingCollection(@RequestBody CollectionStepDTO collectionStepDTO);

	/**
	 * Find by type and enabled.
	 *
	 * @return the list
	 */
	@GetMapping("/setting-notifications/find-by-type-enabled")
	List<SettingNotificationsDTO> findByTypeAndEnabled();

	/**
	 * Find fees.
	 *
	 * @return the list
	 */
	@GetMapping("/settings-list-values/find-fees")
	List<ApplicationFeeDTO> findFees();
	
	/**
	 * Save note from ib.
	 *
	 * @param claimNoteDTO the claim note DTO
	 * @param token the token
	 * @return the claim note DTO
	 */
	@PostMapping("/claim-note/save-note-acm")
	ClaimNoteDTO saveNoteFromIb(@RequestBody ClaimNoteDTO claimNoteDTO, @RequestHeader("Authorization") String token);

}

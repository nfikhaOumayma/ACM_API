/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.exceptions.advice;

import java.net.NoRouteToHostException;

import org.apache.chemistry.opencmis.commons.exceptions.CmisConnectionException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisObjectNotFoundException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisRuntimeException;
import org.apache.chemistry.opencmis.commons.exceptions.CmisUnauthorizedException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.dao.DataAccessException;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.HttpServerErrorException;
import org.springframework.web.client.ResourceAccessException;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestClientResponseException;
import org.springframework.web.client.UnknownHttpStatusCodeException;

import com.acm.constants.common.CommonErrorCode;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.exceptions.model.ExceptionResponseMessage;
import com.acm.exceptions.model.TechnicalException;
import com.acm.exceptions.type.AMLPourcentageConfigurationException;
import com.acm.exceptions.type.ApiAbacusException;
import com.acm.exceptions.type.CalculateAgeEndLoanException;
import com.acm.exceptions.type.CalculateAgeException;
import com.acm.exceptions.type.CancelIssuedLoanException;
import com.acm.exceptions.type.CheckAppL1NotFoundException;
import com.acm.exceptions.type.CheckAppL2NotFoundException;
import com.acm.exceptions.type.CheckAppL3NotFoundException;
import com.acm.exceptions.type.CheckAppL4NotFoundException;
import com.acm.exceptions.type.CheckFeesException;
import com.acm.exceptions.type.CheckFieldsConfigurationException;
import com.acm.exceptions.type.CheckLevelProcessException;
import com.acm.exceptions.type.CheckMezaCardException;
import com.acm.exceptions.type.CheckMezaCardUntrustException;
import com.acm.exceptions.type.CodeSettingExistException;
import com.acm.exceptions.type.CollateralNotFoundException;
import com.acm.exceptions.type.ConditionalApproveException;
import com.acm.exceptions.type.CreditException;
import com.acm.exceptions.type.CustomerContactException;
import com.acm.exceptions.type.CustomerMaxActiveAccountException;
import com.acm.exceptions.type.DisbursementException;
import com.acm.exceptions.type.EnableCriticalDataException;
import com.acm.exceptions.type.ExpenseDrAndCrAccountsEmptyException;
import com.acm.exceptions.type.ExpensesLimitNotFoundException;
import com.acm.exceptions.type.ExpensesTypeUnicityCodeException;
import com.acm.exceptions.type.FieldVisitNotFoundException;
import com.acm.exceptions.type.FinancialAnalysisNotFoundException;
import com.acm.exceptions.type.GEDException;
import com.acm.exceptions.type.GroupeUsersFoundException;
import com.acm.exceptions.type.GuarantorsNotFoundException;
import com.acm.exceptions.type.IScoreExpiryDayException;
import com.acm.exceptions.type.IScoreExpiryDayFailedErrorException;
import com.acm.exceptions.type.IScoreProductConfigurationException;
import com.acm.exceptions.type.IncentiveRegistrationException;
import com.acm.exceptions.type.InformCustomerNotFoundException;
import com.acm.exceptions.type.InitialCheckException;
import com.acm.exceptions.type.JournalEntryException;
import com.acm.exceptions.type.JournalEntryWorkflowStepException;
import com.acm.exceptions.type.LoanAlreadyExistException;
import com.acm.exceptions.type.MezaCardExistException;
import com.acm.exceptions.type.MezaCardsExistInDbException;
import com.acm.exceptions.type.NationalIdNotFoundException;
import com.acm.exceptions.type.OldPwdInvalidException;
import com.acm.exceptions.type.ParametrageException;
import com.acm.exceptions.type.ProductQueryExepction;
import com.acm.exceptions.type.PwdConfirmInvalidException;
import com.acm.exceptions.type.RenewalConditionSettingLoanException;
import com.acm.exceptions.type.ReportingException;
import com.acm.exceptions.type.RequestAlreadyExistException;
import com.acm.exceptions.type.ResetPwdException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.exceptions.type.SaveFileException;
import com.acm.exceptions.type.UploadDocumentNotFoundException;
import com.acm.exceptions.type.UploadSignedDocNotFoundExepction;
import com.acm.exceptions.type.WorkFlowSettingException;
import com.acm.utils.validation.ACMValidationUtils;

import feign.FeignException;

/**
 * {@link ExceptionHandlingControllerAdvice} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@RestControllerAdvice
public class ExceptionHandlingControllerAdvice {

	/** Default Mode is INFO. */
	private static final Logger logger =
			LoggerFactory.getLogger(ExceptionHandlingControllerAdvice.class);

	/**
	 * Resource not found.
	 *
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(ResourcesNotFoundException.class)
	public ResponseEntity<ExceptionResponseMessage> resourceNotFoundHandler(
			ResourcesNotFoundException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.CODE_DATA_NOT_FOUND);
		response.setErrorMessage(exception.getMessage());

		logger.error("Fire resource No tFound Exception type");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * InitialCheck Exception.
	 * 
	 * @author MoezMhiri
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(InitialCheckException.class)
	public ResponseEntity<ExceptionResponseMessage> initialCheckExceptionHandler(
			InitialCheckException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.INITIAL_CHECK_CODE_DATA_NOT_FOUND);
		response.setErrorMessage(exception.getMessage());

		logger.error("Fire Initial Check Exception type");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * CodeSettingExist Exception.
	 * 
	 * @author YesserSomai
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(CodeSettingExistException.class)
	public ResponseEntity<ExceptionResponseMessage> codeSettingExistExceptionHandler(
			CodeSettingExistException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.CODE_SETTING_EXIST);
		response.setErrorMessage(exception.getMessage());

		logger.error("Code Setting ExistException Exception type");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Cmis unauthorized exception handler.
	 * 
	 * @author HaythemBenizid
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(CmisUnauthorizedException.class)
	public ResponseEntity<ExceptionResponseMessage> cmisUnauthorizedExceptionHandler(
			CmisUnauthorizedException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.GED_CONNECTION_ERROR);
		response.setErrorMessage(exception.getMessage());

		logger.error("GED CmisUnauthorizedException type");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Cmis runtime exception handler.
	 * 
	 * @author HaythemBenizid
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(CmisRuntimeException.class)
	public ResponseEntity<ExceptionResponseMessage> cmisRuntimeExceptionHandler(
			CmisRuntimeException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.GED_CONNECTION_ERROR);
		response.setErrorMessage(exception.getMessage());

		logger.error("GED CmisRuntimeException type");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Cmis object not found exception handler.
	 * 
	 * @author HaythemBenizid
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(CmisObjectNotFoundException.class)
	public ResponseEntity<ExceptionResponseMessage> cmisObjectNotFoundExceptionHandler(
			CmisObjectNotFoundException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.GED_CONNECTION_ERROR);
		response.setErrorMessage(exception.getMessage());

		logger.error("GED CmisObjectNotFoundException type");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Cmis connection exception handler.
	 * 
	 * @author HaythemBenizid
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(CmisConnectionException.class)
	public ResponseEntity<ExceptionResponseMessage> cmisConnectionExceptionHandler(
			CmisConnectionException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.GED_CONNECTION_ERROR);
		response.setErrorMessage(exception.getMessage());

		logger.error("GED CmisConnectionException type");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * FieldVisit Not Found.
	 * 
	 * @author MoezMhiri
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(FieldVisitNotFoundException.class)
	public ResponseEntity<ExceptionResponseMessage> fieldVisitNotFoundExceptionHandler(
			FieldVisitNotFoundException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.FIELD_VISIT_CODE_DATA_NOT_FOUND);
		response.setErrorMessage(exception.getMessage());

		logger.error("Fire Field Visit Not Found Exception type");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Guarantors not found.
	 * 
	 * @author MoezMhiri
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(GuarantorsNotFoundException.class)
	public ResponseEntity<ExceptionResponseMessage> guarantorsNotFoundExceptionHandler(
			GuarantorsNotFoundException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.GUARANTOR_CODE_DATA_NOT_FOUND);
		response.setErrorMessage(exception.getMessage());

		logger.error("Fire Guarantors Not Found Exception type");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Collateral not found.
	 * 
	 * @author MoezMhiri
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(CollateralNotFoundException.class)
	public ResponseEntity<ExceptionResponseMessage> collateralNotFoundExceptionHandler(
			CollateralNotFoundException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.COLLATEROL_CODE_DATA_NOT_FOUND);
		response.setErrorMessage(exception.getMessage());

		logger.error("Fire Collateral Not Found Exception type");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Upload Document not found.
	 * 
	 * @author MoezMhiri
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(UploadDocumentNotFoundException.class)
	public ResponseEntity<ExceptionResponseMessage> uploadDocumentNotFoundExceptionHandler(
			UploadDocumentNotFoundException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.UPLOAD_DOCUMENT_CODE_DATA_NOT_FOUND);
		response.setErrorMessage(exception.getMessage());

		logger.error("Fire Upload Document Not Found Exception type");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Financial Analysis not found.
	 * 
	 * @author MoezMhiri
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(FinancialAnalysisNotFoundException.class)
	public ResponseEntity<ExceptionResponseMessage> financialAnalysisNotFoundExceptionHandler(
			FinancialAnalysisNotFoundException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.FINANCIAL_ANALYSIS_CODE_DATA_NOT_FOUND);
		response.setErrorMessage(exception.getMessage());

		logger.error("Fire Financial Analysis Not Found Exception type");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Check Approve L1 not found.
	 * 
	 * @author MoezMhiri
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(CheckAppL1NotFoundException.class)
	public ResponseEntity<ExceptionResponseMessage> checkAppL1NotFoundExceptionHandler(
			CheckAppL1NotFoundException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.CHECK_APP_L1_CODE_DATA_NOT_FOUND);
		response.setErrorMessage(exception.getMessage());

		logger.error("Fire Check App L1 Not Found Exception type");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Check Approve L2 not found.
	 * 
	 * @author MoezMhiri
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(CheckAppL2NotFoundException.class)
	public ResponseEntity<ExceptionResponseMessage> checkAppL2NotFoundExceptionHandler(
			CheckAppL2NotFoundException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.CHECK_APP_L2_CODE_DATA_NOT_FOUND);
		response.setErrorMessage(exception.getMessage());

		logger.error("Fire Check App L2 Not Found Exception type");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Check Approve L3 not found.
	 * 
	 * @author MoezMhiri
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(CheckAppL3NotFoundException.class)
	public ResponseEntity<ExceptionResponseMessage> checkAppL3NotFoundExceptionHandler(
			CheckAppL3NotFoundException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.CHECK_APP_L3_CODE_DATA_NOT_FOUND);
		response.setErrorMessage(exception.getMessage());

		logger.error("Fire Check App L3 Not Found Exception type");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Check Approve L4 not found.
	 * 
	 * @author MoezMhiri
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(CheckAppL4NotFoundException.class)
	public ResponseEntity<ExceptionResponseMessage> checkAppL4NotFoundExceptionHandler(
			CheckAppL4NotFoundException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.CHECK_APP_L4_CODE_DATA_NOT_FOUND);
		response.setErrorMessage(exception.getMessage());

		logger.error("Fire Check App L4 Not Found Exception type");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Inform Customer not found.
	 * 
	 * @author MoezMhiri
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(InformCustomerNotFoundException.class)
	public ResponseEntity<ExceptionResponseMessage> informCustomerNotFoundExceptionHandler(
			InformCustomerNotFoundException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.INFORM_CUSTOMER_CODE_DATA_NOT_FOUND);
		response.setErrorMessage(exception.getMessage());

		logger.error("Fire Inform Customer Not Found Exception type");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Upload Signed Doc not found.
	 * 
	 * @author MoezMhiri
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(UploadSignedDocNotFoundExepction.class)
	public ResponseEntity<ExceptionResponseMessage> uploadSignedDocNotFoundExepction(
			UploadSignedDocNotFoundExepction exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.UPLOAD_SIGNED_DOCUMENT_CODE_DATA_NOT_FOUND);
		response.setErrorMessage(exception.getMessage());

		logger.error("Fire Upload Signed Doc Not Found Exepction type");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Groupe Users found.
	 * 
	 * @author MoezMhiri
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(GroupeUsersFoundException.class)
	public ResponseEntity<ExceptionResponseMessage> usersFoundExceptionHandler(
			GroupeUsersFoundException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.USER_FOUND);
		response.setErrorMessage(exception.getMessage());
		logger.error("Fire Users Found Exepction type");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Resource is null using NullPointerException class.
	 *
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(NullPointerException.class)
	public ResponseEntity<ExceptionResponseMessage> resourceIsNull(NullPointerException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.CODE_DATA_NOT_FOUND);
		response.setErrorMessage("Internal error has been occurred, contact administrator.");

		logger.error("Fire resource Is Null Exception type");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Resource is illegal argument exception.
	 *
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(IllegalArgumentException.class)
	public ResponseEntity<ExceptionResponseMessage> resourceIllegalArgumentExceptionHandler(
			IllegalArgumentException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.CODE_DATA_NOT_FOUND);
		response.setErrorMessage(exception.getMessage());

		logger.error("Fire resource Is IllegalArgument Exception type");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Resource class cast exception handler.
	 *
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(java.lang.ClassCastException.class)
	public ResponseEntity<ExceptionResponseMessage> resourceClassCastExceptionHandler(
			ClassCastException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.CODE_DATA_NOT_FOUND);
		response.setErrorMessage(exception.getMessage());

		logger.error("Fire resource Is ClassCast Exception type");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Data access exception.
	 *
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(DataAccessException.class)
	public ResponseEntity<ExceptionResponseMessage> dataAccessExceptionHandler(
			DataAccessException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.CODE_INVALID_QUERY);
		response.setErrorMessage(
				"Cannot execute this query as it might involve data access problem.");
		logger.error("Fire data Access Exception type");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * No such method exception.
	 *
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(NoSuchMethodException.class)
	public ResponseEntity<ExceptionResponseMessage> noSuchMethodExceptionHandler(
			NoSuchMethodException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.CODE_INVALID_QUERY);
		response.setErrorMessage("NoSuchMethodException");

		logger.error("Fire No Such Method Exception type");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Http client error exception.
	 * 
	 * @author HaythemBenizid
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(HttpClientErrorException.class)
	public ResponseEntity<ExceptionResponseMessage> httpClientErrorExceptionHandler(
			HttpClientErrorException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.FEIGN_CONNECTION_ERROR);
		response.setErrorMessage(
				"httpClientErrorException : " + exception.getResponseBodyAsString());

		logger.error("Fire Http Client Error Exception type");
		return new ResponseEntity<>(response, HttpStatus.INTERNAL_SERVER_ERROR);
	}

	/**
	 * Rest client response exception handler.
	 * 
	 * @author HaythemBenizid
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(RestClientResponseException.class)
	public ResponseEntity<ExceptionResponseMessage> restClientResponseExceptionHandler(
			RestClientResponseException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.FEIGN_CONNECTION_ERROR);
		response.setErrorMessage(exception.getResponseBodyAsString());

		logger.error("Fire RestClientResponseException type");
		return new ResponseEntity<>(response, HttpStatus.INTERNAL_SERVER_ERROR);
	}

	/**
	 * Http server error exception handler.
	 * 
	 * @author HaythemBenizid
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(HttpServerErrorException.class)
	public ResponseEntity<ExceptionResponseMessage> httpServerErrorExceptionHandler(
			HttpServerErrorException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.FEIGN_CONNECTION_ERROR);
		response.setErrorMessage(exception.getResponseBodyAsString());

		logger.error("Fire HttpServerErrorException type");
		return new ResponseEntity<>(response, HttpStatus.INTERNAL_SERVER_ERROR);
	}

	/**
	 * Resource access exception handler.
	 * 
	 * @author HaythemBenizid
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(ResourceAccessException.class)
	public ResponseEntity<ExceptionResponseMessage> resourceAccessExceptionHandler(
			ResourceAccessException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.FEIGN_CONNECTION_ERROR);
		response.setErrorMessage(exception.getMessage());

		logger.error("Fire ResourceAccessException type");
		return new ResponseEntity<>(response, HttpStatus.INTERNAL_SERVER_ERROR);
	}

	/**
	 * Rest client exception handler.
	 * 
	 * @author HaythemBenizid
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(RestClientException.class)
	public ResponseEntity<ExceptionResponseMessage> restClientExceptionHandler(
			RestClientException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.FEIGN_CONNECTION_ERROR);
		response.setErrorMessage(exception.getMessage());

		logger.error("Fire RestClientException type");
		return new ResponseEntity<>(response, HttpStatus.INTERNAL_SERVER_ERROR);
	}

	/**
	 * Unknown http status code exception handler.
	 * 
	 * @author HaythemBenizid
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(UnknownHttpStatusCodeException.class)
	public ResponseEntity<ExceptionResponseMessage> unknownHttpStatusCodeExceptionHandler(
			UnknownHttpStatusCodeException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.FEIGN_CONNECTION_ERROR);
		response.setErrorMessage(exception.getResponseBodyAsString());

		logger.error("Fire UnknownHttpStatusCodeException type");
		return new ResponseEntity<>(response, HttpStatus.INTERNAL_SERVER_ERROR);
	}

	/**
	 * Illegal state exception.
	 *
	 * @author HaythemBenizid
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(IllegalStateException.class)
	public ResponseEntity<ExceptionResponseMessage> illegalStateExceptionHandler(
			IllegalStateException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.CODE_LOAD_BALANCER_NOT_AVAILABLE);
		response.setErrorMessage("illegalStateException : " + exception.getMessage());

		logger.error("Fire illegal State Exception type");
		return new ResponseEntity<>(response, HttpStatus.INTERNAL_SERVER_ERROR);
	}

	/**
	 * (com.netflix.client) Client Exception.
	 *
	 * @author HaythemBenizid
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(com.netflix.client.ClientException.class)
	public ResponseEntity<ExceptionResponseMessage> clientExceptionHandler(
			com.netflix.client.ClientException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.CODE_LOAD_BALANCER_NOT_AVAILABLE);
		response.setErrorMessage(exception.getMessage());

		logger.error("PROXY - Fire com.netflix.client.ClientException type");
		return new ResponseEntity<>(response, HttpStatus.INTERNAL_SERVER_ERROR);
	}

	/**
	 * (java.net.ConnectException) Connect Exception.
	 *
	 * @author HaythemBenizid
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(java.net.ConnectException.class)
	public ResponseEntity<ExceptionResponseMessage> connectExceptionHandler(
			java.net.ConnectException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode("PROXY_CONNECT_EXCEPTION");
		response.setErrorMessage(exception.getMessage());

		logger.error("PROXY - Fire java.net.ConnectException type");
		return new ResponseEntity<>(response, HttpStatus.INTERNAL_SERVER_ERROR);
	}

	/**
	 * Feign exception handler.
	 *
	 * @param exception the feign exception
	 * @return the response entity
	 */
	@ExceptionHandler(FeignException.class)
	public ResponseEntity<ExceptionResponseMessage> feignExceptionHandler(
			FeignException exception) {

		logger.error("Feign exception thrown.");
		return handleCommonExceptions(CommonErrorCode.FEIGN_CONNECTION_ERROR,
				CommonExceptionsMessage.EXCEPTIONS_FAILED_LOAD_BALANCER, exception);
	}

	/**
	 * No route to host exception handler.
	 * 
	 * @author HaythemBenizid
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(NoRouteToHostException.class)
	public ResponseEntity<ExceptionResponseMessage> noRouteToHostExceptionHandler(
			NoRouteToHostException exception) {

		logger.error("No route to host (Host unreachable).");
		return handleCommonExceptions(CommonErrorCode.NO_ROUTE_TO_HOST,
				CommonExceptionsMessage.EXCEPTIONS_NO_ROUTE_TO_HOST, exception);
	}

	/**
	 * handle common exceptions.
	 * 
	 * @author HaythemBenizid
	 * @param errorCode the exception error code
	 * @param errorMessage the error message to be display
	 * @param exception the real exception
	 * @return {@link ExceptionResponseMessage} to be display to side client
	 */
	private ResponseEntity<ExceptionResponseMessage> handleCommonExceptions(String errorCode,
			String errorMessage, Exception exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(errorCode);
		response.setErrorMessage(errorMessage);
		response.setErrorTechnicalMessage(new TechnicalException(CommonErrorCode.CODE_INVALID_DATA,
				exception.getMessage(), null));

		logger.error(exception.getMessage(), exception);
		return new ResponseEntity<>(response, HttpStatus.INTERNAL_SERVER_ERROR);
	}

	/**
	 * CheckLevelProcessException Exception.
	 * 
	 * @author AbdelkarimTurki
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(CheckLevelProcessException.class)
	public ResponseEntity<ExceptionResponseMessage> checkLevelProcessExceptionHandler(
			CheckLevelProcessException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.LEVEL_PROCESS_AMOUNT_NOT_SORTED);
		response.setErrorMessage(exception.getMessage());

		logger.error("Level process Setting Amount Exception");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * ApiAbacusException Exception.
	 * 
	 * @author MoezMhiri
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(ApiAbacusException.class)
	public ResponseEntity<ExceptionResponseMessage> apiAbacusExceptionHandler(
			ApiAbacusException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.API_ABACUS);
		response.setErrorMessage(exception.getMessage());

		logger.error("API ABACUS Exception");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Customer Max Active Account Exception.
	 * 
	 * @author YesserSomai
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(CustomerMaxActiveAccountException.class)
	public ResponseEntity<ExceptionResponseMessage> customerMaxActiveAccountException(
			CustomerMaxActiveAccountException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.CUSTOMER_LIMIT_MAX_ACTIVE_ACCOUNT);
		response.setErrorMessage(exception.getMessage());

		logger.error("Customer Limit Max Active Account Exception");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Calculate age exception.
	 * 
	 * @author HaythemBenizid
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(CalculateAgeException.class)
	public ResponseEntity<ExceptionResponseMessage> calculateAgeException(
			CalculateAgeException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.CUSTOMER_INVALID_DATE_BIRTH);
		response.setErrorMessage(exception.getMessage());

		logger.error("Calculate Age Exception");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Calculate age exception.
	 * 
	 * @author hbeji
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(CalculateAgeEndLoanException.class)
	public ResponseEntity<ExceptionResponseMessage> calculateAgeEndLoanException(
			CalculateAgeEndLoanException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.CUSTOMER_INVALID_AGE);
		response.setErrorMessage(exception.getMessage());

		logger.error("Calculate Age End Loan Exception");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Check fields configuration exception.
	 * 
	 * @author Ines Dridi
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(CheckFieldsConfigurationException.class)
	public ResponseEntity<ExceptionResponseMessage> checkFieldsConfigurationException(
			CheckFieldsConfigurationException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		ExceptionResponseMessage exceptionResponseMessage =
				!ACMValidationUtils.isNullOrEmpty(exception.getExceptionResponseMessage())
						? exception.getExceptionResponseMessage()
						: new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.CHECK_FIELD_CONFIGURATION);
		response.setErrorMessage(exceptionResponseMessage.getErrorMessage());
		if (exceptionResponseMessage.getErrorTechnicalMessage() != null) {
			response.setErrorTechnicalMessage(new TechnicalException(
					exceptionResponseMessage.getErrorCode(),
					exceptionResponseMessage.getErrorTechnicalMessage().getErrorMessage(),
					exceptionResponseMessage.getErrorTechnicalMessage().getClassException()));
		}
		response.setErrors(exceptionResponseMessage.getErrors());
		logger.error(exception.getMessage(), exception);
		logger.error("check Fields Configuration Exception");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Reset pwd exception.
	 * 
	 * @author MoezMhiri
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(ResetPwdException.class)
	public ResponseEntity<ExceptionResponseMessage> resetPwdException(ResetPwdException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.LOGIN_INVALID);
		response.setErrorMessage(exception.getMessage());

		logger.error("Login Invalid Exception");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Old pwd exception.
	 *
	 * @author MoezMhiri
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(OldPwdInvalidException.class)
	public ResponseEntity<ExceptionResponseMessage> oldPwdInvalidException(
			OldPwdInvalidException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.PWD_INVALID);
		response.setErrorMessage(exception.getMessage());

		logger.error("Old password invalid Exception");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Pwd confirm invalid exception.
	 *
	 * @author MoezMhiri
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(PwdConfirmInvalidException.class)
	public ResponseEntity<ExceptionResponseMessage> pwdConfirmInvalidException(
			PwdConfirmInvalidException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.PWD_DONT_MATCH);
		response.setErrorMessage(exception.getMessage());

		logger.error("Password confirmation invalid Exception");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Credit Exception.
	 * 
	 * @author salmen fatnassi
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(CreditException.class)
	public ResponseEntity<ExceptionResponseMessage> creditExceptionHandler(
			CreditException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(exception.getExceptionResponseMessage().getErrorCode());
		response.setErrorMessage(exception.getMessage());

		logger.error("Fire Credit Exception type");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * GED Exception.
	 * 
	 * @author HaythemBenizid
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(GEDException.class)
	public ResponseEntity<ExceptionResponseMessage> gedExceptionHandler(GEDException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.DOCUMENT_ERROR);
		response.setErrorMessage(exception.getMessage());
		response.setErrorTechnicalMessage(
				exception.getExceptionResponseMessage().getErrorTechnicalMessage());

		logger.error("Fire GED Exception type");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * FAILED GENERATE REPORT exception.
	 * 
	 * @author Manel lamloum
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(ReportingException.class)
	public ResponseEntity<ExceptionResponseMessage> reportingException(
			ReportingException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.FAILED_GENERATE_REPORT);
		response.setErrorMessage(exception.getMessage());

		logger.error("Report Exception");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Socket timeout exception.
	 *
	 * @author HaythemBenizid
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(java.net.SocketTimeoutException.class)
	public ResponseEntity<ExceptionResponseMessage> socketTimeoutException(
			java.net.SocketTimeoutException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.CODE_LOAD_BALANCER_NOT_AVAILABLE);
		response.setErrorMessage(exception.getMessage());

		logger.error("Fire java.net.SocketTimeoutException type");
		return new ResponseEntity<>(response, HttpStatus.INTERNAL_SERVER_ERROR);
	}

	/**
	 * Customer contact exception.
	 *
	 * @author ManelLamloum
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(CustomerContactException.class)
	public ResponseEntity<ExceptionResponseMessage> customerContactException(
			CustomerContactException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.NO_CUSTOMER_CONTACT);
		response.setErrorMessage(exception.getMessage());

		logger.error("unable to contact customer Exception");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Save File Exception.
	 *
	 * @author Yesser Somai
	 * @param exception the exception
	 * @return the response entity
	 */

	@ExceptionHandler(SaveFileException.class)
	public ResponseEntity<ExceptionResponseMessage> saveFileException(SaveFileException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.SAVE_FILE_FAILED);
		response.setErrorMessage(exception.getMessage());

		logger.error("Save File Exception");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Incentive exception handler.
	 * 
	 * @author idridi
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(IncentiveRegistrationException.class)
	public ResponseEntity<ExceptionResponseMessage> incentiveExceptionHandler(
			IncentiveRegistrationException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(exception.getExceptionResponseMessage().getErrorCode());
		response.setErrorMessage(exception.getMessage());

		logger.error("Fire Incentive Exception type");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * I score product configuration exception.
	 * 
	 * @author HaythemBenizid
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(IScoreProductConfigurationException.class)
	public ResponseEntity<ExceptionResponseMessage> iScoreProductConfigurationException(
			IScoreProductConfigurationException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.ISCORE_PRODUCT_CONFIGURATION);
		response.setErrorMessage(exception.getMessage());

		logger.error("IScore Product Configuration Exception");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * I score expiry day exception.
	 * 
	 * @author HaythemBenizid
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(IScoreExpiryDayException.class)
	public ResponseEntity<ExceptionResponseMessage> iScoreExpiryDayException(
			IScoreExpiryDayException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.ISCORE_INVALID_EXPIRY_DATE);
		response.setErrorMessage(exception.getMessage());

		logger.error("IScore Expiry Day Exception");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * AML pourcentage configuration exception.
	 * 
	 * @author HaythemBenizid
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(AMLPourcentageConfigurationException.class)
	public ResponseEntity<ExceptionResponseMessage> amlPourcentageConfigurationException(
			AMLPourcentageConfigurationException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.AML_POURCENTAGE_CONFIGURATION);
		response.setErrorMessage(exception.getMessage());

		logger.error("AML Pourcentage Configuration Exception");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Meza card exist exception.
	 * 
	 * @author MOEZ
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(MezaCardExistException.class)
	public ResponseEntity<ExceptionResponseMessage> MezaCardExistException(
			MezaCardExistException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.MEZA_CARD_EXIST);
		response.setErrorMessage(exception.getMessage());

		logger.error("Meza card Exist");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Meza cards exist in db exception.
	 * 
	 * @author idridi
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(MezaCardsExistInDbException.class)
	public ResponseEntity<ExceptionResponseMessage> MezaCardsExistInDbException(
			MezaCardsExistInDbException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.CARDS_EXIST_IN_DATABASE);
		response.setErrorMessage(exception.getMessage());

		logger.error("Meza cards already Exist in database");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Loan already exist exception.
	 * 
	 * @author idridi
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(LoanAlreadyExistException.class)
	public ResponseEntity<ExceptionResponseMessage> loanAlreadyExistException(
			LoanAlreadyExistException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.LOAN_ALREADY_EXIST_IN_ACM);
		response.setErrorMessage(exception.getMessage());

		logger.error("Loan Already Exist in ACM");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Cancel issued loan exception.
	 * 
	 * @author idridi
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(CancelIssuedLoanException.class)
	public ResponseEntity<ExceptionResponseMessage> cancelIssuedLoanException(
			CancelIssuedLoanException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.CANNOT_CANCEL_ISSUED_LOAN);
		response.setErrorMessage(exception.getMessage());

		logger.error("Cannot cancel issued loan");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Renewal condition setting loan exception.
	 * 
	 * @author idridi
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(RenewalConditionSettingLoanException.class)
	public ResponseEntity<ExceptionResponseMessage> RenewalConditionSettingLoanException(
			RenewalConditionSettingLoanException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.RENEWAL_CONDITION_SETTING_NOT_FOUND);
		response.setErrorMessage(exception.getMessage());

		logger.error("No Renewal Condition Setting found for this customer");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * I score expiry day failed error exception.
	 * 
	 * @author MoezMhiri
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(IScoreExpiryDayFailedErrorException.class)
	public ResponseEntity<ExceptionResponseMessage> IScoreExpiryDayFailedErrorException(
			IScoreExpiryDayFailedErrorException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.ISCORE_INVALID_EXPIRY_DATE_FAILED_ERROR);
		response.setErrorMessage(exception.getMessage());

		logger.error("IScore Expiry Day Exception");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Request already exist exception.
	 * 
	 * @author ManelLamloum
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(RequestAlreadyExistException.class)
	public ResponseEntity<ExceptionResponseMessage> RequestAlreadyExistException(
			RequestAlreadyExistException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.OPENED_REQUEST_ALREADY_EXIST);
		response.setErrorMessage(exception.getMessage());
		logger.error("Opened Request Exception already exist");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Disbursement exception.
	 *
	 * @author yesser.somai
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(DisbursementException.class)
	public ResponseEntity<ExceptionResponseMessage> DisbursementException(
			DisbursementException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.LOAN_ALREADY_ISSUED);
		response.setErrorMessage(exception.getMessage());
		logger.error("LOAN ALREADY ISSUER");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * National id not found exception.
	 *
	 * @author mlamloum
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(NationalIdNotFoundException.class)
	public ResponseEntity<ExceptionResponseMessage> NationalIdNotFoundException(
			NationalIdNotFoundException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.NATIONAL_ID_NOT_FOUND);
		response.setErrorMessage(exception.getMessage());
		response.setErrors(exception.getExceptionResponseMessage().getErrors());
		logger.error("NATIONAL ID NOT FOUND");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Enable critical data exception.
	 * 
	 * @author idridi
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(EnableCriticalDataException.class)
	public ResponseEntity<ExceptionResponseMessage> enableCriticalDataException(
			EnableCriticalDataException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.ENABLE_CRITICAL_DATA);
		response.setErrorMessage(exception.getMessage());

		logger.error("Error while updating enable critical data for customer");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Workflow setting exception used in save steps.
	 *
	 * @author yesser somai
	 * @param exception the workFlow Setting Exception
	 * @return the response entity
	 */
	@ExceptionHandler(WorkFlowSettingException.class)
	public ResponseEntity<ExceptionResponseMessage> workFlowSettingException(
			WorkFlowSettingException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.WORKFLOW_SETTING_EXCEPTION);
		response.setErrorMessage(exception.getMessage());

		logger.error("Error while updating WorkFlow setting");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);

	}

	/**
	 * Product query exepction.
	 *
	 * @author moezMhiri
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(ProductQueryExepction.class)
	public ResponseEntity<ExceptionResponseMessage> productQueryExepction(
			ProductQueryExepction exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.ERROR_QUERY_GET_PRODUCT_FROM_ABACUS);
		response.setErrorMessage(exception.getMessage());

		logger.error("Error while executing query to get Product from abacus");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Parametrage exception.
	 *
	 * @author mlamloum
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(ParametrageException.class)
	public ResponseEntity<ExceptionResponseMessage> parametrageException(
			ParametrageException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.PRODUCT_NOT_FOUND);
		response.setErrorMessage(exception.getMessage());

		logger.error("Error product not found");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Conditionnal approve exception handler.
	 *
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(ConditionalApproveException.class)
	public ResponseEntity<ExceptionResponseMessage> conditionnalApproveExceptionHandler(
			ConditionalApproveException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.APPROVAL_EXCEPTION);
		response.setErrorMessage(exception.getMessage());

		logger.error("APPROVAL EXCEPTION");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Expenses type unicity code exception handler.
	 *
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(ExpensesTypeUnicityCodeException.class)
	public ResponseEntity<ExceptionResponseMessage> expensesTypeUnicityCodeExceptionHandler(
			ExpensesTypeUnicityCodeException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.UNIQUE_CODE_EXPENSES_TYPE);
		response.setErrorMessage(exception.getMessage());
		logger.error("EXPENSES TYPE UNIQUE CODE EXCEPTION");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Expenses limit not found handler.
	 *
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(ExpensesLimitNotFoundException.class)
	public ResponseEntity<ExceptionResponseMessage> expensesLimitNotFoundHandler(
			ExpensesLimitNotFoundException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.EXPENSES_LIMIT_NOT_FOUND);
		response.setErrorMessage(exception.getMessage());

		logger.error("Fire resource No tFound Exception type");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Expense dr and cr accounts empty handler.
	 *
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(ExpenseDrAndCrAccountsEmptyException.class)
	public ResponseEntity<ExceptionResponseMessage> expenseDrAndCrAccountsEmptyHandler(
			ExpenseDrAndCrAccountsEmptyException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.DrAndCrAccountsEmpty);
		response.setErrorMessage(exception.getMessage());

		logger.error("Fire resource No tFound Exception type");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Journal entry exception handler.
	 *
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(JournalEntryException.class)
	public ResponseEntity<ExceptionResponseMessage> journalEntryExceptionHandler(
			JournalEntryException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.JOURNAL_ENTRY_EXCEPTION);
		response.setErrorMessage(exception.getMessage());

		logger.error("JOURNAL ENTRY SETTING EXCEPTION");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Check meza card exception handler.
	 *
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(CheckMezaCardException.class)
	public ResponseEntity<ExceptionResponseMessage> checkMezaCardExceptionHandler(
			CheckMezaCardException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.CHECK_MEZA_CARD_EXCEPTION);
		response.setErrorMessage(exception.getMessage());

		logger.error("MEZA CARD EXCEPTION");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Check meza card untrust exception.
	 *
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(CheckMezaCardUntrustException.class)
	public ResponseEntity<ExceptionResponseMessage> checkMezaCardUntrustException(
			CheckMezaCardUntrustException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.CHECK_MEZA_CARD_EXCEPTION_UNTRUST);
		response.setErrorMessage(exception.getMessage());

		logger.error("MEZA CARD EXCEPTION UNTRUST");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}

	/**
	 * Check fees exception.
	 *
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(CheckFeesException.class)
	public ResponseEntity<ExceptionResponseMessage> checkFeesException(
			CheckFeesException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.CHECK_FEES_EXCEPTION);
		response.setErrorMessage(exception.getMessage());

		logger.error("CHECK FEES EXCEPTION");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}
	
	/**
	 * Journal entry workflow step exception handler.
	 *
	 * @param exception the exception
	 * @return the response entity
	 */
	@ExceptionHandler(JournalEntryWorkflowStepException.class)
	public ResponseEntity<ExceptionResponseMessage> journalEntryWorkflowStepExceptionHandler(
			JournalEntryWorkflowStepException exception) {

		ExceptionResponseMessage response = new ExceptionResponseMessage();
		response.setErrorCode(CommonErrorCode.JOURNAL_ENTRY_WORKFLOW_STEP);
		response.setErrorMessage(exception.getMessage());

		logger.error("JOURNAL ENTRY TYPE EXCEPTION");
		return new ResponseEntity<>(response, HttpStatus.BAD_REQUEST);
	}


}

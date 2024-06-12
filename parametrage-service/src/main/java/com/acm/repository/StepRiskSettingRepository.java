package com.acm.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.acm.utils.models.StepRiskSetting;
import com.acm.utils.models.WorkFlowStep;

/**
 * The Interface StepRiskSettingRepository.
 */
@Repository
public interface StepRiskSettingRepository extends JpaRepository<StepRiskSetting, Long> {

	/**
	 * Find by work flow step.
	 *
	 * @param workFlowStep the work flow step
	 * @return the list
	 */
	List<StepRiskSetting> findByWorkFlowStep(WorkFlowStep workFlowStep);

	/**
	 * Find by work flow step process and work flow step enabled.
	 *
	 * @param string the string
	 * @param b the b
	 * @return the list
	 */
	List<StepRiskSetting> findByWorkFlowStepProcessAndWorkFlowStepEnabled(String string, boolean b);

	/**
	 * Find by work flow step process and work flow step enabled and product id.
	 *
	 * @param genericWorkflowWorkflowProcess the generic workflow workflow process
	 * @param b the b
	 * @param id the id
	 * @return the list
	 */

	List<StepRiskSetting> findByWorkFlowStepProcessAndWorkFlowStepEnabledAndWorkFlowStepProductId(
			String genericWorkflowWorkflowProcess, boolean b, Long id);

}

<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    width="600px"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="90px">
      <el-form-item label="工序类型" prop="sequenceType">
        <common-radio-button
          v-model="form.sequenceType"
          :options="typeEnum.ENUM"
          type="enum"
          :disabled-val="disabledList"
          size="small"
          @change="typeChnage(form.sequenceType)"
        />
      </el-form-item>
      <el-form-item v-if="form.sequenceType === typeEnum.ARTIFACT.V" label="工序次序" prop="type">
        <common-radio-button v-model="form.type" :options="processTypeEnum.ENUM" size="small" type="enum" />
      </el-form-item>
      <el-form-item label="上报类型" prop="reportType">
        <template v-slot:label>
          上报类型
          <el-tooltip
            class="item"
            effect="light"
            placement="top"
            :content="`检验方式设四种方式：\n
          单件（不扫码）：每次只能上报一个，不需要扫码；\n
          单件（需扫码）：每次只能上报一个，需要扫码；\n
          批量（不扫码）：每次可以上报多个，不需要扫码;\n
          批量（需扫码）：每次可以上报多个，需要扫码。`"
          >
            <i class="el-icon-info" />
          </el-tooltip>
        </template>
        <common-radio-button v-model="form.reportType" :options="reportTypeEnum.ENUM" :disabled-val="reportDisabled" type="enum" size="small" />
      </el-form-item>
      <el-form-item label="检验类型" prop="inspectType">
        <template v-slot:label>
          检验类型
          <el-tooltip
            class="item"
            effect="light"
            placement="top"
            :content="`检验方式设四种方式：\n
          单件（不扫码）：每次只能检验一个，不需要扫码；\n
          单件（需扫码）：每次只能检验一个，需要扫码；\n
          批量（不扫码）：每次可以检验多个，不需要扫码;\n
          批量（需扫码）：每次可以检验多个，需要扫码。`"
          >
            <i class="el-icon-info" />
          </el-tooltip>
        </template>
        <common-radio-button
          v-model="form.inspectType"
          :options="inspectTypeEnum.ENUM"
          :disabled-val="inspectDisabled"
          type="enum"
          size="small"
        />
      </el-form-item>
      <el-form-item label="工序名称" prop="name">
        <el-input v-model="form.name" type="text" placeholder="请填写工序名称" style="width: 270px" />
      </el-form-item>
      <el-form-item label="排序" prop="sort">
        <el-input-number v-model.number="form.sort" :min="1" :max="999" :step="1" controls-position="right" style="width: 270px" />
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { computed, ref } from 'vue'
import {
  processTypeEnum,
  processMaterialListTypeEnum as typeEnum,
  processInspectTypeEnum as inspectTypeEnum,
  processReportTypeEnum as reportTypeEnum
} from '@enum-ms/mes'
import { regForm } from '@compos/use-crud'

const formRef = ref()

const defaultForm = {
  id: undefined,
  name: '',
  sort: 1,
  inspectType: inspectTypeEnum.BATCH_SCAN.V,
  reportType: reportTypeEnum.BATCH_SCAN.V,
  sequenceType: typeEnum.ARTIFACT.V,
  type: processTypeEnum.ONCE.V
}

const { crud, form } = regForm(defaultForm, formRef)

const rules = {
  sort: [{ required: true, message: '请填写排序值', trigger: 'blur', type: 'number' }],
  name: [
    { required: true, message: '请填写工序名称', trigger: 'blur' },
    { min: 1, max: 32, message: '长度在 1 到 32 个字符', trigger: 'blur' }
  ]
}

const reportDisabled = ref([])
const inspectDisabled = ref([])

const disabledList = computed(() => {
  return form.id
    ? [typeEnum.ARTIFACT.V, typeEnum.MACHINE_PART.V, typeEnum.ENCLOSURE.V].filter((v) => v !== form.sequenceType)
    : [typeEnum.ENCLOSURE.V]
})

function typeChnage(sequenceType) {
  if (sequenceType === typeEnum.MACHINE_PART.V) {
    form.reportType = reportTypeEnum.BATCH_UNSCAN.V
    form.inspectType = inspectTypeEnum.BATCH_UNSCAN.V
    reportDisabled.value = [reportTypeEnum.BATCH_SCAN.V, reportTypeEnum.SINGLE_SCAN.V]
    inspectDisabled.value = [inspectTypeEnum.BATCH_SCAN.V, inspectTypeEnum.SINGLE_SCAN.V]
  } else {
    form.reportType = reportTypeEnum.BATCH_SCAN.V
    form.inspectType = inspectTypeEnum.BATCH_SCAN.V
    reportDisabled.value = []
    inspectDisabled.value = []
  }
  if (sequenceType === typeEnum.ARTIFACT.V) {
    form.type = processTypeEnum.ONCE.V
  } else {
    delete form.type
  }
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
</style>

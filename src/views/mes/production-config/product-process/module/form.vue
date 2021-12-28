<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelCU"
    :visible="crud.status.cu > 0"
    :title="crud.status.title"
    width="500px"
  >
    <template #titleRight>
      <common-button :loading="crud.status.cu === 2" type="primary" size="mini" @click="crud.submitCU">确认</common-button>
    </template>
    <el-form ref="formRef" :model="form" :rules="rules" size="small" label-width="90px">
      <el-form-item label="工序类型" prop="sequenceType">
        <common-radio-button
          v-model="form.sequenceType"
          :options="typeEnum.ENUM"
          :unshow-val="[typeEnum.MACHINE_PART.V]"
          type="enum"
          size="small"
          @change="productTypeChange(form.sequenceType)"
        />
      </el-form-item>
      <el-form-item v-if="form.sequenceType === typeEnum.ARTIFACT.V" label="工序次序" prop="processType">
        <common-radio-button v-model="form.processType" :options="processTypeEnum.ENUM" size="small" type="enum" />
      </el-form-item>
      <el-form-item label="名称" prop="name">
        <el-input v-model="form.name" type="text" :placeholder="`请填写${typeEnum.VL[form.sequenceType]}类型名称`" style="width: 270px" />
      </el-form-item>
      <el-form-item label="工序" prop="processSequenceIds">
        <div class="process-container">
          <div class="process-box">
            <div v-for="(item, index) in form.processSequenceIds" :key="index" class="process-drawer">
              <process-select
                ref="processSelectRef"
                v-model="form.processSequenceIds[index]"
                :size="'small'"
                :multiple="false"
                :clearable="true"
                :product-type="productType"
                style="width: 220px;"
                :disabled-value="processDisabled(form.processSequenceIds, form.processSequenceIds[index])"
              />
              <common-button
                v-show="form.processSequenceIds && form.processSequenceIds.length > 1"
                icon="el-icon-delete"
                size="mini"
                type="danger"
                style="margin-left:3px;"
                @click="delProcess(index)"
              />
              <common-button v-show="index === form.processSequenceIds.length-1" icon="el-icon-plus" size="mini" type="success" @click="addProcess" />
            </div>
          </div>
        </div>
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { computed, ref } from 'vue'
import { ElMessageBox, ElMessage } from 'element-plus'

import { processTypeEnum, processMaterialListTypeEnum as typeEnum, componentTypeEnum } from '@enum-ms/mes'
import { arrIsRepeat } from '@data-type/array'
import { arr2obj } from '@/utils/convert/type'

import { regForm } from '@compos/use-crud'
import processSelect from '@comp-mes/process-select'
import { isNotBlank } from '@/utils/data-type'

const formRef = ref()
const processSelectRef = ref()

const defaultForm = {
  id: undefined,
  name: '',
  processSequenceIds: [undefined],
  sequenceType: typeEnum.ARTIFACT.V,
  processType: processTypeEnum.ONCE.V
}

const { crud, form, CRUD } = regForm(defaultForm, formRef)

const rules = {
  name: [
    { required: true, message: '请填写类型名称', trigger: 'blur' },
    { min: 1, max: 32, message: '长度在 1 到 32 个字符', trigger: 'blur' }
  ],
  processSequenceIds: [{ required: true, message: '请选择工序' }]
}

const productType = computed(() => {
  let _type = form.sequenceType
  if (isNotBlank(form.processType) && form.processType === processTypeEnum.ONCE.V) {
    _type = componentTypeEnum.ASSEMBLE.V
  }
  return _type
})

function productTypeChange(sequenceType) {
  if (sequenceType === typeEnum.ARTIFACT.V) {
    form.processType = processTypeEnum.ONCE.V
  } else {
    delete form.processType
  }
}

// 工序禁用
function processDisabled(ids, currentId) {
  return ids.filter((id) => id !== currentId)
}

function addProcess() {
  form.processSequenceIds.push(undefined)
}

function delProcess(index) {
  form.processSequenceIds.splice(index, 1)
}

// 验证前
CRUD.HOOK.afterValidateCU = () => {
  const processFlag =
    crud.form.processSequenceIds && crud.form.processSequenceIds.length > 0 && !crud.form.processSequenceIds.some((v) => !v && v !== 0)
  if (!processFlag) {
    ElMessage({
      message: `请正确填写${typeEnum.VL[crud.form.sequenceType]}工序信息`,
      type: 'error'
    })
  }
  return processFlag
}

// 提交前
CRUD.HOOK.beforeSubmit = async () => {
  const isRepeat = arrIsRepeat(crud.form.processSequenceIds)
  const sourceData = await processSelectRef.value.getSourceData()
  const processArr = arr2obj(sourceData.value, 'id')
  const processSequence = crud.form.processSequenceIds.map((id) => `【${processArr[id].name}】`).join('→')
  try {
    await ElMessageBox.confirm(
      `“${crud.form.name}”的工序为：\n${processSequence}\n${isRepeat ? '检测到重复工序，' : ''}确认提交？`,
      '提示',
      {
        confirmButtonText: '确定',
        cancelButtonText: '取消',
        type: 'warning'
      }
    )
    const processSequenceIds = []
    crud.form.processSequenceIds.forEach((v, index) => {
      processSequenceIds.push({
        processId: v,
        productProcessId: crud.form.id,
        sequence: index
      })
    })
    crud.form.medBuildingProductProcessLinkList = processSequenceIds
    if (crud.form.sequenceType === typeEnum.ENCLOSURE.V) {
      crud.form.processType = processTypeEnum.TWICE.V
    }
    return true
  } catch (error) {
    return false
  }
}
</script>

<style rel="stylesheet/scss" lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}

.process-container {
  display: flex;
  flex-direction: row;
  justify-content: flex-start;
  align-items: flex-end;

  ::v-deep(.el-button--mini) {
    padding: 5px 10px;
  }
  .process-box {
    display: flex;
    flex-direction: column;
    justify-content: space-between;
    align-items: flex-start;
    .process-drawer {
      display: flex;
      flex-direction: row;
      justify-content: flex-start;
      align-items: center;
      margin-bottom: 10px;
    }
  }
}
</style>

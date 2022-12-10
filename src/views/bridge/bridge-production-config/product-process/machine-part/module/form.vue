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
    <el-form ref="formRef" :model="form" size="small" label-width="90px">
      <el-form-item label="零件类型">
        <span>{{ form.name }}</span>
      </el-form-item>
      <el-form-item label="工序" prop="processSequenceIds">
        <div class="process-container">
          <div class="process-box">
            <div v-for="(item, index) in form.processSequenceIds" :key="index" class="process-drawer">
              <process-select
                :ref="(el) => (processSelectRef[index] = el)"
                v-model="form.processSequenceIds[index]"
                :size="'small'"
                :multiple="false"
                :clearable="true"
                :product-type="form.productType"
                style="width: 220px"
                :disabled-value="processDisabled(form.processSequenceIds, form.processSequenceIds[index])"
              />
              <common-button
                v-show="form.processSequenceIds && form.processSequenceIds.length > 1 && index > 0"
                icon="el-icon-delete"
                size="mini"
                type="danger"
                style="margin-left: 3px"
                @click="delProcess(index)"
              />
              <common-button
                v-show="index === form.processSequenceIds.length - 1"
                icon="el-icon-plus"
                size="mini"
                type="success"
                @click="addProcess"
              />
            </div>
          </div>
        </div>
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { ref } from 'vue'
import { ElMessageBox } from 'element-plus'

import { bridgeProcessTypeEnum as typeEnum } from '@enum-ms/bridge'
import { arrIsRepeat } from '@data-type/array'
import { cleanArray } from '@/utils/data-type/array'
import { arr2obj } from '@/utils/convert/type'

import useProcess from '@compos/store/use-bridge-process'
import { regForm } from '@compos/use-crud'
import processSelect from '@/components-system/bridge/process-select'

const { process } = useProcess()

const formRef = ref()
const processSelectRef = ref([])

const defaultForm = {
  id: undefined,
  processSequenceIds: [undefined]
}

const { crud, form, CRUD } = regForm(defaultForm, formRef)

// const rules = {
//   processSequenceIds: [{ required: true, message: '请选择工序' }]
// }

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

CRUD.HOOK.beforeToCU = () => {
  if (!form.processSequenceIds?.length) {
    form.processSequenceIds = [undefined]
  }
  form.productType = typeEnum.MACHINE_PART.V
  const _mProcess = (process.value?.length && process.value?.filter((v) => v.productType && v.productType & form.productType)) || []
  if (_mProcess?.length) {
    form.processSequenceIds[0] = _mProcess[0].id
  }
}

// 验证前
// CRUD.HOOK.afterValidateCU = () => {
//   const processFlag = form.processSequenceIds && form.processSequenceIds.length > 0 && !form.processSequenceIds.some((v) => !v && v !== 0)
//   if (!processFlag) {
//     ElMessage({
//       message: `请正确填写${typeEnum.VL[form.productType]}工序信息`,
//       type: 'error'
//     })
//   }
//   return processFlag
// }

// 提交前
CRUD.HOOK.beforeSubmit = async () => {
  const _processSequenceIds = cleanArray(form.processSequenceIds)
  const isRepeat = arrIsRepeat(_processSequenceIds)
  const sourceData = await processSelectRef.value[0].getSourceData()
  const processArr = arr2obj(sourceData.value, 'id')
  const processSequence = _processSequenceIds.map((id) => `【${processArr[id].name}】`).join('→')
  try {
    await ElMessageBox.confirm(
      `“${form.name}”的工序为：\n${processSequence || '-'}\n${isRepeat ? '检测到重复工序，' : ''}确认提交？`,
      '提示',
      {
        confirmButtonText: '确定',
        cancelButtonText: '取消',
        type: 'warning'
      }
    )
    const processSequenceIds = []
    _processSequenceIds.forEach((v, index) => {
      processSequenceIds.push({
        id: v,
        nodeTime: 0,
        sequence: index
      })
    })
    form.processLinkList = processSequenceIds
    form.typeId = form.id
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

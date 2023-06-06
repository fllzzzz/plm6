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
      <el-form-item label="分段类型">
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
                style="width: 180px"
                class="input-underline"
                :disabled-value="processDisabled(form.processSequenceIds, form.processSequenceIds[index])"
              />
              <template v-if="index !== form.processSequenceIds.length - 1">
                <common-input-number
                  v-model="processSequenceObj[form.processSequenceIds[index]]"
                  :step="1"
                  :controls="false"
                  placeholder="耗时"
                  :min="0"
                  :max="99"
                  size="small"
                  class="input-underline"
                  style="width: 60px; margin-left: 3px"
                />
                <span style="margin-left: 3px">天</span>
              </template>
              <common-button
                v-show="form.processSequenceIds && form.processSequenceIds.length > 1"
                icon="el-icon-delete"
                size="mini"
                type="danger"
                style="margin-left: 10px"
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
import { isBlank, isNotBlank } from '@data-type/index'
import { arr2obj } from '@/utils/convert/type'

import { regForm } from '@compos/use-crud'
import processSelect from '@/components-system/bridge/process-select'

const formRef = ref()
const processSelectRef = ref([])

const defaultForm = {
  id: undefined,
  processSequenceIds: [undefined]
}
const processSequenceObj = ref({})

const { crud, form, CRUD } = regForm(defaultForm, formRef)

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
  if (isNotBlank(form.processSequenceObj)) {
    processSequenceObj.value = form.processSequenceObj
  } else {
    processSequenceObj.value = {}
  }
  form.productType = typeEnum.BOX.V
}

// 提交前
CRUD.HOOK.beforeSubmit = async () => {
  if (form.processSequenceIds.length === 1 && isBlank(form.processSequenceIds[0])) {
    form.typeId = form.id
    return true
  }
  const isRepeat = arrIsRepeat(form.processSequenceIds)
  const sourceData = await processSelectRef.value[0].getSourceData()
  const processArr = arr2obj(sourceData.value, 'id')
  const processSequence = form.processSequenceIds
    .map((id) => `【${processArr[id].name}】${processSequenceObj.value[id] ? '→ ' + processSequenceObj.value[id] + '天 ' : ''}`)
    .join(`→`)
  try {
    await ElMessageBox.confirm(`“${form.name}”的工序为：\n${processSequence}\n${isRepeat ? '检测到重复工序，' : ''}确认提交？`, '提示', {
      confirmButtonText: '确定',
      cancelButtonText: '取消',
      type: 'warning'
    })
    const processSequenceIds = []
    form.processSequenceIds.forEach((v, index) => {
      processSequenceIds.push({
        id: v,
        nodeTime: processSequenceObj.value[v] || 0,
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

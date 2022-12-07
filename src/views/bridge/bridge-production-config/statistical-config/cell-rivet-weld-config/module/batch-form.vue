<template>
  <common-dialog
    fullscreen
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelBCU"
    :visible="dialogVisible"
    :title="`${crud.bStatus.title}`"
    :show-close="false"
    custom-class="rivet-weld-config-batch-add"
    @opened="handleOpen"
    top="10vh"
  >
    <template #titleRight>
      <common-button size="mini" type="success" icon="el-icon-plus" @click="addRow(form.list)" />
      <common-button :loading="crud.bStatus.cu === CRUD.STATUS.PROCESSING" type="primary" size="mini" @click="crud.submitBCU">
        提 交
      </common-button>
      <!-- <store-operation type="crudBatch" /> -->
      <common-button size="mini" @click="crud.cancelBCU">关 闭</common-button>
    </template>
    <el-form ref="formRef" :model="form" :disabled="submitLoading">
      <common-table
        ref="table"
        :data="form.list"
        :show-empty-symbol="false"
        v-loading="tableLoading"
        return-source-data
        empty-text="暂无数据"
        :max-height="maxHeight"
        default-expand-all
        :cell-class-name="wrongCellMask"
        row-key="uid"
      >
        <el-table-column label="序号" type="index" align="center" width="60" fixed="left" />
        <el-table-column prop="sysAssembleId" :show-overflow-tooltip="true" label="部件类型" width="150" align="center" fixed="left">
          <template #default="{ row, $index }">
            <common-select
              v-model="row.sysAssembleId"
              :options="assembleTypeList"
              :show-extra="$index !== 0"
              type="other"
              class="input-underline"
              :dataStructure="{ key: 'id', label: 'name', value: 'id' }"
              placeholder="部件类型"
              style="width: 100%"
              @change="handleClassificationChange($event, $index)"
            />
          </template>
        </el-table-column>
        <el-table-column prop="specPrefixStr" :show-overflow-tooltip="true" label="截面前缀" width="150" align="center" fixed="left">
          <template #default="{ row, $index }">
            <span v-if="row.sysAssembleId === -1 && $index !== 0">同上</span>
            <span v-else>{{ row.sysAssembleId && assembleTypeListObj[row.sysAssembleId]?.specPrefixStr }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="numerical" :show-overflow-tooltip="true" label="数值" width="200" align="center" fixed="left">
          <template #default="{ row }">
            <common-input-number
              v-model="row.minNumerical"
              :step="1"
              :min="0"
              :precision="2"
              clearable
              :controls="false"
              size="mini"
              class="input-underline"
              placeholder="最小数值"
              style="width: 45%"
            />
            <span> ~ </span>
            <common-input-number
              v-model="row.maxNumerical"
              :step="1"
              :precision="2"
              class="input-underline"
              :controls="false"
              size="mini"
              clearable
              placeholder="最大数值"
              style="width: 45%"
            />
          </template>
        </el-table-column>
        <el-table-column :show-overflow-tooltip="true" label="工序单价（元）" align="center" min-width="200px">
          <el-table-column
            v-for="item in processList"
            :key="item.id"
            :show-overflow-tooltip="true"
            align="center"
            :label="item.name"
            width="240px"
          >
            <template #default="{ row, $index }">
              <template v-if="isNotBlank(row?.processObj[item.id])">
                <common-select
                  v-model="row.processObj[item.id].wageQuotaType"
                  :options="wageQuotaTypeEnum.ENUM"
                  type="enum"
                  clearable
                  size="mini"
                  class="input-underline"
                  placeholder="计量方式"
                  style="width: 45%; margin-right: 5px"
                  @change="wageQuotaTypeChange($event, $index, item.id)"
                />
                <common-input-number
                  v-model="row.processObj[item.id].price"
                  :step="1"
                  :min="0"
                  :precision="2"
                  clearable
                  class="input-underline"
                  :controls="false"
                  size="mini"
                  placeholder="单价"
                  style="width: 30%"
                />
                <span v-if="row?.processObj[item.id]?.wageQuotaType">
                  {{ wageQuotaTypeEnum.V[row.processObj[item.id].wageQuotaType].unit }}
                </span>
              </template>
              <span v-else>-</span>
            </template>
          </el-table-column>
        </el-table-column>
        <el-table-column min-width="1px" />
        <el-table-column label="操作" width="70px" align="center" fixed="right">
          <template #default="{ $index }">
            <common-button
              type="danger"
              icon="el-icon-delete"
              size="mini"
              style="padding: 6px"
              @click.stop="removeRow(form.list, $index)"
            />
          </template>
        </el-table-column>
      </common-table>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { defineProps, ref, reactive, nextTick, inject, computed } from 'vue'

import { isNotBlank, deepClone } from '@data-type/index'
import { createUniqueString } from '@/utils/data-type/string'
import { wageQuotaTypeEnum } from '@enum-ms/mes'
import { obj2arr } from '@/utils/convert/type'

import { regBatchForm } from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import useTableValidate from '@compos/form/use-table-validate'
// import StoreOperation from '@crud/STORE.operation.vue'

defineProps({
  modelValue: {
    type: Boolean,
    require: true
  }
})

const processList = inject('processList')
// const processListObj = inject('processListObj')
const assembleTypeList = inject('assembleTypeList')
const assembleTypeListObj = inject('assembleTypeListObj')

// 提交loading
const submitLoading = ref(false)
// 刷新表格
const tableLoading = ref(true)

// 表单
const defaultForm = reactive({
  list: [] // 添加列表
})

const validateNumerical = (value, row) => {
  if (!row.minNumerical || !row.maxNumerical || row.maxNumerical < row.minNumerical) return false
  return true
}

const rules = {
  sysAssembleId: [{ required: true, message: '请选择部件类型', trigger: 'change' }],
  numerical: [{ validator: validateNumerical, message: '请填写数值并且最大数值不得小于最小数值', trigger: 'blur' }]
}

// 同上的选项与值
const ditto = new Map([['sysAssembleId', -1]])
const rowNumbers = 5

const formRef = ref()

const { CRUD, crud, form } = regBatchForm(defaultForm, formRef)
const dialogVisible = computed(() => crud.bStatus.cu > CRUD.STATUS.NORMAL)

const { tableValidate, cleanUpData, wrongCellMask } = useTableValidate({ rules, ditto })

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.rivet-weld-config-batch-add',
    extraBox: ['.el-dialog__header'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    navbar: false
  },
  dialogVisible
)

function handleOpen() {
  init(form.list)
  tableLoading.value = true
  nextTick(() => {
    tableLoading.value = false
  })
}

// 表单校验
CRUD.HOOK.beforeValidateBCU = () => {
  // TODO: 目前无需校验
  const { validResult, dealList } = tableValidate(form.list)
  if (validResult) {
    form.list = dealList
  } else {
    return validResult
  }
}

// 表单提交数据清理
crud.submitBatchFormFormat = async (form) => {
  cleanUpData(form.list)
  const formList = form.list.map((row) => {
    return {
      sysAssembleId: row.sysAssembleId,
      maxNumerical: row.maxNumerical,
      minNumerical: row.minNumerical,
      structureProcessPriceList: obj2arr(row.processObj)
    }
  })
  return formList
}

const tableProcessObj = computed(() => {
  const _obj = {}
  if (!form.list?.length) return _obj
  let curSysAssembleId = null
  for (let i = 0; i < form.list.length; i++) {
    curSysAssembleId = form.list[i].sysAssembleId !== -1 ? form.list[i].sysAssembleId : curSysAssembleId
    _obj[i] = curSysAssembleId
  }
  return _obj
})

function handleClassificationChange(val, index) {
  let _processObj = {}
  if (isNotBlank(assembleTypeListObj.value[val]?.typeProcessObj)) {
    _processObj = assembleTypeListObj.value[val]?.typeProcessObj
  }
  for (const item in tableProcessObj.value) {
    if (tableProcessObj.value[item] === val) {
      form.list[item].processObj = deepClone(_processObj)
    }
  }
}

function wageQuotaTypeChange(val, index, processId) {
  for (const item in tableProcessObj.value) {
    if (tableProcessObj.value[item] === tableProcessObj.value[index]) {
      form.list[Number(item)].processObj[processId].wageQuotaType = val
    }
  }
}

// 初始行
const init = (list) => {
  list.length = 0
  for (let i = 0; i < rowNumbers; i++) {
    addRow(list)
  }
}

// 添加行
const addRow = (list) => {
  const row = {
    processObj: {}
  }
  row.uid = createUniqueString()
  if (ditto && list.length > 0) {
    ditto.forEach((value, key) => {
      row[key] = value
    })
  }
  if (list.length && isNotBlank(list[list.length - 1].processObj)) {
    row.processObj = deepClone(list[list.length - 1].processObj)
  }
  list.push(row)
}

// 删除行
const removeRow = (list, index) => {
  list.splice(index, 1)
}
</script>

<style lang="scss" scoped>
.rivet-weld-config-batch-add {
  ::v-deep(.el-dialog__body) {
    padding-top: 0px;
  }
  .header-operate {
    margin-bottom: 15px;
  }
}
</style>

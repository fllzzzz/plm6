<template>
  <common-dialog
    append-to-body
    :close-on-click-modal="false"
    :before-close="crud.cancelBCU"
    :visible="dialogVisible"
    :title="crud.bStatus.title"
    :show-close="false"
    fullscreen
    custom-class="inventory-warning-batch-add"
    width="700px"
    top="10vh"
  >
    <template #titleRight>
      <common-button :loading="crud.bStatus.cu === CRUD.STATUS.PROCESSING" type="primary" size="mini" @click="crud.submitBCU">
        提 交
      </common-button>
      <store-operation type="crudBatch" @clear="handleClear" />
      <common-button size="mini" @click="crud.cancelBCU">关 闭</common-button>
    </template>
    <div class="flex-rss">
      <material-spec-select ref="specRef" class="spec-select" v-model="form.list" :row-init-fn="rowInit" :max-height="maxHeight + 42" />
      <div style="width: 100%">
        <div class="setting-container">
          <div class="setting-item">
            <el-tag size="medium" effect="plain">【批量】设置预警数量</el-tag>
            <el-input-number
              v-model.number="batch.minimumInventory"
              :min="0"
              :max="999999"
              :step="1"
              :precision="3"
              size="small"
              controls-position="right"
              placeholder="预警数量"
              style="width: 200px"
            />
            <common-button :disabled="!crud.selections.length" type="success" size="small" @click="handleBatchSetNumber">
              设置
            </common-button>
          </div>
          <div class="setting-item">
            <el-tag size="medium" effect="plain">【批量】设置工厂</el-tag>
            <factory-select v-model="batch.factoryId" placeholder="可选择工厂" clearable />
            <common-button :disabled="!crud.selections.length" type="success" size="small" @click="handleBatchSetFactory">
              设置
            </common-button>
          </div>
        </div>
        <el-form ref="formRef" :model="form" :disabled="crud.bStatus.cu === CRUD.STATUS.PROCESSING">
          <common-table
            ref="tableRef"
            border
            :data="form.list"
            :max-height="maxHeight"
            :cell-class-name="wrongCellMask"
            @selection-change="crud.selectionChangeHandler"
          >
            <el-table-column type="selection" width="55" align="center" />
            <el-table-column label="序号" type="index" align="center" width="60" />
            <el-table-column
              key="classifyFullName"
              :show-overflow-tooltip="true"
              prop="classifyFullName"
              label="科目"
              align="left"
              min-width="200"
            />
            <el-table-column
              key="specification"
              :show-overflow-tooltip="true"
              prop="specification"
              label="规格"
              align="left"
              min-width="200"
            />
            <el-table-column key="unitType" :show-overflow-tooltip="true" prop="unitType" label="单位配置" align="center" width="140">
              <template #default="{ row }">
                <common-radio-button
                  v-model="row.unitType"
                  :options="measureTypeEnum.ENUM"
                  :disabled-val="row.unitTypeDisabled"
                  type="enum"
                  size="small"
                />
              </template>
            </el-table-column>
            <el-table-column key="unit" :show-overflow-tooltip="true" prop="unit" label="单位" align="center" width="70">
              <template #default="{ row }">
                {{ row.unitType === measureTypeEnum.MEASURE.V ? row.measureUnit : row.accountingUnit }}
              </template>
            </el-table-column>
            <el-table-column
              key="minimumInventory"
              :show-overflow-tooltip="true"
              prop="minimumInventory"
              label="数量"
              align="center"
              min-width="120"
            >
              <template #default="{ row }">
                <el-input-number
                  v-model="row.minimumInventory"
                  :min="0"
                  :max="999999"
                  :step="1"
                  :precision="row.unitType === measureTypeEnum.MEASURE.V ? row.measurePrecision : row.accountingPrecision"
                  size="small"
                  controls-position="right"
                  style="width: 100%; max-width: 200px"
                />
              </template>
            </el-table-column>
            <el-table-column key="factoryId" prop="factoryId" label="工厂" align="center" min-width="160">
              <template #default="{ row }">
                <factory-select v-model="row.factoryId" placeholder="可选择工厂" clearable style="width: 100%" />
              </template>
            </el-table-column>
            <el-table-column key="operate" :show-overflow-tooltip="true" prop="operate" label="操作" align="center" width="70">
              <template #default="{ row, $index }">
                <common-button icon="el-icon-delete" type="danger" size="mini" @click="delRow(row.sn, $index)" />
              </template>
            </el-table-column>
          </common-table>
        </el-form>
      </div>
    </div>
  </common-dialog>
</template>

<script setup>
import { reactive, ref, computed } from 'vue'
import { measureTypeEnum } from '@enum-ms/wms'
import { isNotBlank, isBlank } from '@/utils/data-type'

import { regBatchForm } from '@compos/use-crud'
import useTableValidate from '@compos/form/use-table-validate'
import useMaxHeight from '@compos/use-max-height'
import StoreOperation from '@crud/STORE.operation.vue'
import materialSpecSelect from '@comp-cls/material-spec-select/index.vue'
import factorySelect from '@comp-base/factory-select.vue'
import { numFmtByUnitForList } from '@/utils/wms/convert-unit'

// 未进行重复数据校验，目前由后端处理
const tableRules = {}
const defaultForm = {
  list: []
}

const batch = reactive({
  minimumInventory: undefined,
  factoryId: undefined
})
const tableRef = ref()
const formRef = ref()
const specRef = ref()

const { CRUD, crud, form } = regBatchForm(defaultForm, formRef)
const dialogVisible = computed(() => crud.bStatus.cu > CRUD.STATUS.NORMAL)

const { tableValidate, cleanUpData, wrongCellMask } = useTableValidate({ rules: tableRules })

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.inventory-warning-batch-add',
    extraBox: ['.el-dialog__header', '.setting-container'],
    wrapperBox: ['.el-dialog__body'],
    clientHRepMainH: true,
    navbar: false
  },
  dialogVisible
)

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

// 打开时初始化组件
CRUD.HOOK.beforeToBCU = () => specInit()

// 表单提交数据清理
crud.submitBatchFormFormat = async (form) => {
  cleanUpData(form.list)
  const formList = form.list.map((row) => {
    return {
      classifyId: row.classifyId,
      specification: row.specification,
      specificationMap: row.specificationMap,
      minimumInventory: row.minimumInventory,
      unit: row.unitType === measureTypeEnum.MEASURE.V ? row.measureUnit : row.accountingUnit,
      unitPrecision: row.unitType === measureTypeEnum.MEASURE.V ? row.measurePrecision : row.accountingPrecision,
      unitType: row.unitType,
      factoryId: row.factoryId
    }
  })
  await numFmtByUnitForList(formList, {
    fields: ['minimumInventory'],
    toSmallest: true,
    toNum: true
  })
  return formList
}

function handleClear() {
  specRef.value && specRef.value.clear()
}

function specInit() {
  specRef.value && specRef.value.init()
}

// 行初始化
function rowInit(row) {
  const _row = {
    sn: row.sn, // 该科目规格唯一编号
    classifyId: row.classify.id, // 科目id
    classifyFullName: row.classify.fullName, // 全路径名称
    specification: row.spec, // 规格
    specificationMap: row.specKV, // 规格KV格式
    unitType: row.classify.outboundUnitType, // 单位配置
    unitTypeDisabled: [], // 禁用单位
    measureUnit: row.classify.measureUnit, // 计量单位
    accountingUnit: row.classify.accountingUnit, // 核算单位
    accountingPrecision: row.classify.accountingPrecision, // 核算单位小数精度
    measurePrecision: row.classify.measurePrecision, // 计量单位小数精度
    minimumInventory: 0, // 最低库存数量
    factoryId: undefined // 工厂
  }
  if (isBlank(_row.measureUnit)) {
    _row.unitTypeDisabled.push(measureTypeEnum.MEASURE.V)
  }
  if (isBlank(_row.accountingUnit)) {
    _row.unitTypeDisabled.push(measureTypeEnum.ACCOUNTING.V)
  }
  return _row
}

// 删除行
function delRow(sn, index) {
  specRef.value.delListItem(sn, index)
}

// 批量设置预警数量
function handleBatchSetNumber() {
  if (isNotBlank(crud.selections)) {
    crud.selections.forEach((v) => {
      v.minimumInventory = batch.minimumInventory
    })
  }
}

// 批量设置工厂
function handleBatchSetFactory() {
  if (isNotBlank(crud.selections)) {
    crud.selections.forEach((v) => {
      v.factoryId = batch.factoryId
    })
  }
}
</script>

<style lang="scss" scoped>
.spec-select {
  width: 500px;
  margin-right: 5px;
}
.setting-container {
  margin-bottom: 10px;
  .setting-item {
    display: inline-flex;
    align-items: center;
    .el-tag.el-tag--medium {
      height: 32px;
      line-height: 28px;
    }
  }

  .setting-item + .setting-item {
    margin-left: 30px;
  }
  .setting-item > :nth-child(n) {
    margin-right: 6px;
  }
  .setting-item > :last-child {
    margin-right: 0px;
  }
}
</style>

<template>
  <common-table
    ref="tableRef"
    v-bind="$attrs"
    :data="form.auxMatList"
    :cell-class-name="wrongCellMask"
    :expand-row-keys="expandRowKeys"
    :show-empty-symbol="false"
    return-source-data
    row-key="uid"
    @select="selectTableChange"
    @select-all="selectAllTableChange"
  >
    <el-table-column v-if="!props.boolPartyA && !props.noDetail" type="selection" width="55" align="center" :selectable="selectable" />
    <el-expand-table-column :data="form.auxMatList" v-model:expand-row-keys="expandRowKeys" row-key="uid" fixed="left">
      <template #default="{ row }">
        <div class="mtb-10" style="margin-left: 30px">
          <el-input
            v-model="row.remark"
            :rows="1"
            :autosize="{ minRows: 1, maxRows: 1 }"
            type="textarea"
            placeholder="备注"
            maxlength="200"
            show-word-limit
            style="width: 400px"
          />
        </div>
        <div v-if="isNotBlank(row.inboundList)" class="flex-rsc mtb-20" style="margin-left: 30px">
          <inbound-info-table :stripe="false" :material="row" :basic-class="basicClass" :list="row.inboundList" style="width: 1600px" />
        </div>
      </template>
    </el-expand-table-column>
    <el-table-column label="序号" type="index" align="center" width="60" fixed="left" />
    <el-table-column prop="serialNumber" label="编号" align="center" min-width="110px" fixed="left" show-overflow-tooltip />
    <el-table-column prop="classifyName" label="物料种类" align="center" fixed="left" min-width="150" show-overflow-tooltip>
      <template #default="{ row }">
        <el-tooltip :content="row.classifyParentFullName" :disabled="!row.classifyParentFullName" :show-after="500" placement="top">
          <span>{{ row.classifyName || '-'}}</span>
        </el-tooltip>
      </template>
    </el-table-column>
    <el-table-column prop="specification" label="规格" align="center" min-width="200px" fixed="left" show-overflow-tooltip>
      <template #default="{ row }">
        <el-tooltip :content="row.specificationLabels" placement="top">
          <span>{{row.specification || '-'}}</span>
        </el-tooltip>
      </template>
    </el-table-column>
    <el-table-column prop="measureUnit" label="计量单位" align="center" min-width="70px">
      <template #default="{ row }">
        <span>{{ row.measureUnit || '-' }}</span>
      </template>
    </el-table-column>
    <template v-if="props.boolPartyA || props.noDetail">
      <el-table-column prop="quantity" label="数量" align="center" min-width="120px">
        <template #default="{ row }">
          <common-input-number
            v-if="row.measureUnit"
            v-model="row.quantity"
            :min="0"
            :max="999999999"
            :controls="false"
            :step="1"
            :precision="row.measurePrecision"
            size="mini"
            placeholder="数量"
          />
          <span v-else v-empty-text />
        </template>
      </el-table-column>
    </template>
    <template v-else>
      <el-table-column prop="purchaseQuantity" :label="`采购数量`" align="right" min-width="100px">
        <template #default="{ row }">
          <span v-if="row.measureUnit">
            <el-tooltip effect="dark" content="已入库数量" placement="top">
              <span class="color-green">{{ row.inboundQuantity }}</span>
            </el-tooltip>
            / {{ row.purchaseQuantity }}
          </span>
          <span v-else v-empty-text />
        </template>
      </el-table-column>
    </template>
    <el-table-column prop="accountingUnit" label="核算单位" align="center" min-width="70px">
      <template #default="{ row }">
        <span>{{ row.accountingUnit || '-' }}</span>
      </template>
    </el-table-column>
    <template v-if="props.boolPartyA || props.noDetail">
      <el-table-column prop="mete" label="核算量" align="center" min-width="120px">
        <template #default="{ row }">
          <common-input-number
            v-model="row.mete"
            :min="0.000001"
            :max="999999999"
            :controls="false"
            :step="1"
            :precision="row.accountingPrecision"
            size="mini"
            placeholder="核算量"
          />
        </template>
      </el-table-column>
    </template>
    <template v-else>
      <el-table-column prop="purchaseMete" :label="`采购量`" align="right" min-width="100px">
        <template #default="{ row }">
          <span>
            <el-tooltip effect="dark" content="已入库量" placement="top">
              <span class="color-green">{{ row.inboundMete }}</span>
            </el-tooltip>
            / {{ row.purchaseMete }}
          </span>
        </template>
      </el-table-column>
    </template>

    <!-- 金额设置 -->
    <price-set-columns v-if="!props.boolPartyA && fillableAmount && noDetail" weight-attribute="mete" />

    <el-table-column prop="color" label="颜色" align="center" min-width="120px">
      <template #default="{ row }">
        <el-input
          v-if="(props.boolPartyA || props.noDetail) || (!props.boolPartyA && !props.noDetail && form.selectObj?.[row.mergeId]?.isSelected)"
          v-model.trim="row.color"
          maxlength="20"
          size="mini"
          placeholder="颜色"
        />
        <span v-else>{{ row.color || '-' }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="brand" label="品牌" align="center" min-width="120px">
      <template #default="{ row }">
        <el-input
          v-if="(props.boolPartyA || props.noDetail) || (!props.boolPartyA && !props.noDetail && form.selectObj?.[row.mergeId]?.isSelected)"
          v-model.trim="row.brand"
          maxlength="60"
          size="mini"
          placeholder="品牌"
        />
        <span v-else>{{ row.brand || '-'}}</span>
      </template>
    </el-table-column>
    <template v-if="!props.boolPartyA && !props.noDetail">
      <el-table-column prop="quantity" label="本次实收数" align="center" min-width="120px">
        <template #default="{ row }">
          <common-input-number
            v-if="
              !boolApplyPurchase &&
              row.measureUnit &&
              form.selectObj?.[row.mergeId]?.isSelected &&
              row.outboundUnitType === measureTypeEnum.MEASURE.V
            "
            v-model="row.quantity"
            :min="0"
            :max="999999999"
            :controls="false"
            :step="1"
            :precision="row.measurePrecision"
            size="mini"
            placeholder="本次实收数"
            @change="handleQuantityChange(row, row)"
            @blur="handleOverQuantity(row)"
          />
          <span v-else>{{ row.quantity || '-' }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="mete" label="本次实收量" align="center" min-width="120px">
        <template #default="{ row }">
          <common-input-number
            v-if="!boolApplyPurchase && form.selectObj?.[row.mergeId]?.isSelected && row.outboundUnitType === measureTypeEnum.ACCOUNTING.V"
            v-model="row.mete"
            :min="0.000001"
            :max="999999999"
            :controls="false"
            :step="1"
            :precision="row.accountingPrecision"
            size="mini"
            placeholder="实收量"
            @blur="handleOverMete(row)"
          />
          <span v-else>{{ row.mete || '-' }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="boolApplyPurchase" label="操作" width="70" align="center" fixed="right">
        <template #default="{ row }">
          <el-popover placement="top" :width="900" trigger="click">
            <template #reference>
              <span style="margin-left: 5px; cursor: pointer">
                <el-icon-edit v-if="form.selectObj?.[row.mergeId]?.isSelected" class="el-icon" style="color: #1881ef; vertical-align: middle" />
                <el-icon-view v-else class="el-icon" style="color: #1881ef; vertical-align: middle" />
              </span>
            </template>
            <common-table :data="row.applyPurchase" style="width: 100%" return-source-data :show-empty-symbol="false">
              <el-table-column label="序号" type="index" align="center" width="60" />
              <el-table-column prop="serialNumber" :show-overflow-tooltip="true" label="申购单号" min-width="140" align="center" />
              <el-table-column prop="project" :show-overflow-tooltip="true" label="项目" min-width="140" align="center">
                <template #default="{ row: purRow }">
                  <span v-if="purRow.project">{{ projectNameFormatter(purRow.project) }}</span>
                  <span v-else>-</span>
                </template>
              </el-table-column>
              <el-table-column prop="purchaseQuantity" :label="`采购数量`" align="right" min-width="100px">
                <template #default="{ row }">
                  <span>
                    <el-tooltip effect="dark" content="已入库数量" placement="top">
                      <span class="color-green">{{ row.inboundQuantity }}</span>
                    </el-tooltip>
                    / {{ row.purchaseQuantity }}
                  </span>
                </template>
              </el-table-column>
              <el-table-column prop="purchaseMete" :label="`采购量`" align="right" min-width="100px">
                <template #default="{ row }">
                  <span>
                    <el-tooltip effect="dark" content="已入库量" placement="top">
                      <span class="color-green">{{ row.inboundMete }}</span>
                    </el-tooltip>
                    / {{ row.purchaseMete }}
                  </span>
                </template>
              </el-table-column>
              <el-table-column prop="quantity" label="本次实收数" align="center" min-width="120px">
                <template #default="{ row: purRow }">
                  <common-input-number
                    v-if="
                      row.measureUnit && form.selectObj?.[row.mergeId]?.isSelected && row.outboundUnitType === measureTypeEnum.MEASURE.V
                    "
                    v-model="purRow.quantity"
                    :min="0"
                    :max="999999999"
                    :controls="false"
                    :step="1"
                    :precision="row.measurePrecision"
                    size="mini"
                    placeholder="本次实收数"
                    @change="handleQuantityChange(purRow, row)"
                    @blur="handleOverQuantity(purRow)"
                  />
                  <span v-else>{{ purRow.quantity || '-' }}</span>
                </template>
              </el-table-column>
              <el-table-column prop="mete" label="本次实收量" align="center" min-width="120px">
                <template #default="{ row: purRow }">
                  <common-input-number
                    v-if="form.selectObj?.[row.mergeId]?.isSelected && row.outboundUnitType === measureTypeEnum.ACCOUNTING.V"
                    v-model="purRow.mete"
                    :min="0.000001"
                    :max="999999999"
                    :controls="false"
                    :step="1"
                    :precision="row.accountingPrecision"
                    size="mini"
                    placeholder="实收量"
                    @blur="handleOverMete(purRow)"
                  />
                  <span v-else>{{ purRow.mete || '-' }}</span>
                </template>
              </el-table-column>
            </common-table>
          </el-popover>
        </template>
      </el-table-column>
    </template>
    <el-table-column v-if="props.boolPartyA || props.noDetail" label="操作" width="70" align="center" fixed="right">
      <template #default="{ row, $index }">
        <common-button icon="el-icon-delete" type="danger" size="mini" @click="delRow(row.sn, $index)" />
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { defineExpose, defineProps, watch, computed, ref, inject, reactive } from 'vue'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { measureTypeEnum } from '@/utils/enum/modules/wms'
import { createUniqueString } from '@/utils/data-type/string'
import { positiveNumPattern } from '@/utils/validate/pattern'
import { isNotBlank, toPrecision } from '@/utils/data-type'
import { projectNameFormatter } from '@/utils/project'
import { DP } from '@/settings/config'

// import useWmsConfig from '@/composables/store/use-wms-config'
import { regExtra } from '@/composables/form/use-form'
import useTableValidate from '@compos/form/use-table-validate'
import useOverReceive from '@/views/wms/material-inbound/raw-material/application/composables/use-over-receive.js'
import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'

import inboundInfoTable from '@/views/wms/material-inbound/raw-material/components/inbound-info-table'
import priceSetColumns from '@/views/wms/material-inbound/raw-material/components/price-set-columns.vue'

const props = defineProps({
  boolPartyA: {
    type: Boolean,
    default: false
  },
  boolApplyPurchase: {
    type: Boolean,
    default: false
  },
  fillableAmount: {
    type: Boolean,
    default: false
  },
  basicClass: {
    type: [String, Number],
    default: matClsEnum.MATERIAL.V
  },
  noDetail: {
    type: Boolean,
    default: false
  }
})

// 当前物料基础类型
// const basicClass = matClsEnum.MATERIAL.V

// const { purchaseCfg: currentCfg } = useWmsConfig()

const tableRef = ref()
const matSpecRef = inject('matSpecRef') // 调用父组件matSpecRef
const { form } = regExtra() // 表单
const expandRowKeys = ref([]) // 展开行key

// 数量校验方式
const validateQuantity = (value, row) => {
  if (row.measureUnit) return !!value

  return true
}

const rules = {
  classifyId: [{ required: true, message: '请选择物料种类', trigger: 'change' }],
  quantity: [{ validator: validateQuantity, message: '请填写数量', trigger: 'blur' }],
  mete: [
    { required: true, message: '请填写核算量', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '核算量必须大于0', trigger: 'blur' }
  ]
}

// 金额校验
const validateAmount = (value, row) => {
  if (isNotBlank(row.mete) && isNotBlank(row.unitPrice)) {
    return +toPrecision(row.mete * row.unitPrice, DP.YUAN) === +value
  }
  return false
}

// 甲供不需要填写价格
const amountRules = {
  unitPrice: [{ required: true, message: '请填写单价', trigger: 'blur' }],
  amount: [
    { required: true, message: '请填写金额', trigger: 'blur' },
    { validator: validateAmount, message: '金额有误，请手动修改', trigger: 'blur' }
  ]
}

const tableRules = computed(() => {
  let _rules = Object.assign({}, rules)
  if (!props.boolPartyA && props.fillableAmount) {
    _rules = Object.assign(_rules, amountRules)
  }
  return _rules
})

const { tableValidate, wrongCellMask } = useTableValidate({ rules: tableRules }) // 表格校验
const { handleOverQuantity, handleOverMete } = useOverReceive({ meteField: 'mete' })

function selectable(row, rowIndex) {
  return !!row.canPurchaseQuantity || true
}

function selectTableChange(select, row) {
  const boolSelect = Boolean(select.findIndex((v) => v.id === row.id) !== -1)
  form.selectObj[row.mergeId].isSelected = boolSelect
}

function selectAllTableChange(select) {
  const boolSelect = Boolean(select?.length)
  form.auxMatList.forEach((v) => {
    form.selectObj[v.mergeId].isSelected = boolSelect
  })
}
// 设置选择的回显
function setSelect(_rowWatch = false) {
  form.auxMatList.forEach((v) => {
    if (form.selectObj?.[v.mergeId]?.isSelected) {
      tableRef.value.toggleRowSelection(v, true)
    }
  })
}

// 行初始化
function rowInit(row) {
  const _row = reactive({
    uid: createUniqueString(),
    sn: row.sn, // 该科目规格唯一编号
    specificationLabels: row.specificationLabels, // 规格中文
    serialNumber: row.serialNumber, // 科目编号 - 规格
    classifyId: row.classify.id, // 科目id
    classifyFullPathId: row.classify.fullPathId, // 全路径id
    classifyFullName: row.classify.fullName, // 全路径名称
    classifyName: row.classify.name, // 当前科目名称
    classifyParentFullName: row.classify.parentFullName, // 父级路径名称
    basicClass: row.classify.basicClass, // 基础类型
    specification: row.spec, // 规格
    specificationMap: row.specKV, // 规格KV格式
    measureUnit: row.classify.measureUnit, // 计量单位
    accountingUnit: row.classify.accountingUnit, // 核算单位
    accountingPrecision: row.classify.accountingPrecision, // 核算单位小数精度
    measurePrecision: row.classify.measurePrecision, // 计量单位小数精度
    outboundUnitType: row.classify.outboundUnitType, // 出库单位类型
    unitNet: row.unitNet, // 单位净重
    accountingUnitNet: row.accountingUnitNet, // 核算单位净重
    mete: undefined, // 核算量
    quantity: undefined // 数量
  })

  // 非甲供
  if (!props.boolPartyA && props.fillableAmount) {
    _row.unitPrice = undefined // 含税单价
    _row.amount = undefined // 金额
  }
  return _row
}

function rowWatch(row) {
  watch(
    [() => row.boolApplyPurchase, () => row?.applyPurchase],
    () => {
      if (row.boolApplyPurchase && form.selectObj?.[row.mergeId]?.isSelected) {
        row.quantity = row?.applyPurchase?.reduce((a, b) => a + (b.quantity || 0), 0)
        row.mete = row?.applyPurchase?.reduce((a, b) => a + (b.mete || 0), 0)
      }
    },
    { deep: true }
  )
  watch(
    () => row,
    () => {
      if (!props.boolPartyA && !props.noDetail && form.selectObj?.[row.mergeId]?.isSelected) {
        const _isSelected = form.selectObj[row.mergeId]?.isSelected
        form.selectObj[row.mergeId] = {
          ...form.selectObj[row.mergeId],
          ...row,
          isSelected: _isSelected
        }
      }
    },
    { deep: true }
  )
  // 核算量变化
  watch(
    () => row.mete,
    () => {
      handleWeightChange(row.mete, row)
    }
  )
}

// 处理重量变化
function handleWeightChange(val, row, _row) {
  if (isNotBlank(row.unitPrice) && isNotBlank(val)) {
    row.amount = toPrecision(val * row.unitPrice, DP.YUAN)
  }
  handleMeteChange(row, _row || row)
}

// 删除行
function delRow(sn, $index) {
  if (matSpecRef.value) {
    matSpecRef.value.delListItem(sn, $index)
  } else {
    form.auxMatList.splice($index, 1)
  }
}

function handleQuantityChange(row, { unitNet, measurePrecision }) {
  if (!props.boolPartyA) {
    row.mete = toPrecision(row.quantity * unitNet, measurePrecision)
  }
}

function handleMeteChange(row, { accountingUnitNet, accountingPrecision }) {
  // 非甲供 并且 存在计量单位
  if (!props.boolPartyA && row.measureUnit) {
    row.quantity = toPrecision(row.mete * accountingUnitNet, accountingPrecision)
  }
}

// 校验
function validate() {
  const _list = form.auxMatList.filter((v) => {
    if (props.boolPartyA || props.noDetail || form.selectObj[v.mergeId]?.isSelected) {
      return true
    } else {
      return false
    }
  })
  const { validResult } = tableValidate(_list)
  // form.auxMatList = dealList
  return validResult
}

function toggleRowSelection(row, selected) {
  tableRef?.value?.toggleRowSelection(row, selected)
}

defineExpose({
  rowInit,
  toggleRowSelection,
  setSelect,
  rowWatch,
  validate
})
</script>

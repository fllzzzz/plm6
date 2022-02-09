<template>
  <div class="app-container">
    <common-header
      ref="headerRef"
      :edit="props.edit"
      :basic-class="basicClass"
      :list="form.list"
      :current-source="currentSource"
      @add="rowWatch"
    />
    <el-form ref="formRef" :model="form" :disabled="cu.status.edit === FORM.STATUS.PROCESSING">
      <common-table
        ref="tableRef"
        :data="form.list"
        :max-height="maxHeight"
        :default-expand-all="false"
        :stripe="false"
        :cell-class-name="wrongCellMask"
        :expand-row-keys="expandRowKeys"
        row-key="uid"
        highlight-current-row
        @row-click="handleRowClick"
      >
        <el-expand-table-column :data="form.list" v-model:expand-row-keys="expandRowKeys" row-key="uid" fixed="left">
          <template #default="{ row }">
            <div class="mtb-10">
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
          </template>
        </el-expand-table-column>
        <!-- 基础信息 -->
        <material-base-info-columns :basic-class="basicClass" field="source" fixed="left" show-project party-a-position="project" />
        <!-- 次要信息 -->
        <material-secondary-info-columns :basic-class="basicClass" field="source" fixed="left" />

        <el-table-column prop="measureUnit" label="计量单位" align="center" min-width="70px">
          <template #default="{ row }">
            <span v-empty-text>{{ row.measureUnit }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="quantity" label="数量" align="center" min-width="120px">
          <template #default="{ row }">
            <template v-if="row.measureUnit">
              <common-input-number
                v-if="row.outboundUnitType === measureTypeEnum.MEASURE.V"
                v-model="row.quantity"
                :min="0"
                :max="+row.source.quantity"
                :controls="false"
                :step="1"
                :precision="+row.measurePrecision"
                size="mini"
                placeholder="数量"
                @change="handleQuantityChange(row, $event)"
              />
              <span v-else v-to-fixed="{ val: row.quantity || 0, dp: row.measurePrecision }" />
            </template>
            <span v-else v-empty-text />
          </template>
        </el-table-column>
        <el-table-column prop="accountingUnit" label="核算单位" align="center" min-width="70px">
          <template #default="{ row }">
            <span v-empty-text>{{ row.accountingUnit }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="mete" label="核算量" align="center" min-width="120px">
          <template #default="{ row }">
            <common-input-number
              v-if="row.outboundUnitType === measureTypeEnum.ACCOUNTING.V"
              v-model="row.mete"
              :min="0"
              :max="+row.source.sourceReturnableMete"
              :controls="false"
              :step="1"
              :precision="row.accountingPrecision"
              size="mini"
              placeholder="核算量"
              @change="handleMeteChange(row, $event)"
            />
            <span v-else v-to-fixed="{ val: row.mete || 0, dp: row.accountingPrecision }" />
          </template>
        </el-table-column>
        <!-- 仓库设置 -->
        <warehouse-set-columns :list="form.list" />
        <el-table-column label="操作" width="70" align="center" fixed="right">
          <template #default="{ row, $index }">
            <common-button icon="el-icon-delete" type="danger" size="mini" @click="delRow(row, $index)" />
          </template>
        </el-table-column>
      </common-table>
    </el-form>
  </div>
</template>

<script setup>
import { gasReturnApplication } from '@/api/wms/material-return/raw-material/application'
import { edit as editReturnApplication } from '@/api/wms/material-return/raw-material/record'

import { ref, watch, defineEmits, defineProps, reactive, nextTick } from 'vue'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import { measureTypeEnum } from '@/utils/enum/modules/wms'
import { isNotBlank, toFixed } from '@/utils/data-type'

import useMaxHeight from '@compos/use-max-height'
import useForm from '@/composables/form/use-form'
import MaterialBaseInfoColumns from '@/components-system/wms/table-custom-field-columns/material-base-info-columns/index.vue'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-custom-field-columns/material-secondary-info-columns/index.vue'
import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import WarehouseSetColumns from '../components/warehouse-set-columns.vue'
import CommonHeader from '../components/common-header.vue'
import useCurrentRow from '../composables/use-current-row'
import useFormSet from '../composables/use-form-set'
import useCommonCalc from '../composables/use-common-calc'
import { positiveNumPattern } from '@/utils/validate/pattern'

const emit = defineEmits(['success'])

const props = defineProps({
  edit: {
    type: Boolean,
    default: false
  },
  detail: {
    type: Object
  }
})

// 权限
const permission = ['wms_gasReturnApplication:submit']
// 默认表单
const defaultForm = {
  list: []
}

const expandRowKeys = ref([]) // 展开行key
const headerRef = ref()
const tableRef = ref()
const formRef = ref()
// 最大高度
const { fixMaxHeight, maxHeight } = useMaxHeight({ paginate: false })
// 气体
const basicClass = rawMatClsEnum.GAS.V

// 数量校验方式
const validateQuantity = (value, row) => {
  if (row.measureUnit) return !!value
  return true
}

const tableRules = {
  mete: [
    { required: true, message: '请填写核算量' },
    { pattern: positiveNumPattern, message: '核算量必须大于0' }
  ],
  quantity: [{ validator: validateQuantity, message: '有计量单位，数量必须大于0' }],
  factoryId: [{ required: true, message: '请选择工厂' }],
  warehouseId: [{ required: true, message: '请选择存储位置' }]
}

const { cu, form, FORM } = useForm(
  {
    title: '气体退库',
    formStore: !props.edit,
    formStoreKey: 'WMS_RETURN_APPLICATION_GAS',
    permission: permission,
    defaultForm: defaultForm,
    useDraftCallback: setFormCallback,
    clearDraftCallback: init,
    api: props.edit ? editReturnApplication : gasReturnApplication
  },
  formRef,
  props.detail
)

// 通用计算校验
const { calcMaxMete, extractSource, checkOverSource, initCheckOverMaxWeight } = useCommonCalc({ cu, form, basicClass })

// 高亮行处理
const { currentSource, currentUid, delRow, handleRowClick } = useCurrentRow({ form, tableRef, delCallback: checkOverSource })
// 表单信息及校验
const { wrongCellMask } = useFormSet({
  FORM,
  form,
  cu,
  emit,
  tableRules,
  init,
  setFormCallback,
  isEdit: props.edit
})

// 初始化
function init() {
  form.list = []
  // 当前数据
  currentSource.value = undefined
  // 当前高亮uid
  currentUid.value = undefined
  // 异常列表
  cu.props.abnormalList = undefined
}

// 添加材质
function rowWatch(row) {
  // 计算最大总重
  watch([() => row.quantity], () => {
    calcMaxMete(row)
  })
  watch(
    () => row.mete,
    () => {
      checkOverSource(row)
    },
    { immediate: true }
  )
}

// 数量变更
function handleQuantityChange(row, nVal) {
  // 单位净量
  if (isNotBlank(nVal) && row.source.unitNet) {
    row.mete = +toFixed(nVal * row.source.unitNet, row.accountingPrecision)
  } else {
    row.mete = undefined
  }
}

// 核算量变更
function handleMeteChange(row, nVal) {
  if (row.measureUnit && isNotBlank(nVal) && isNotBlank(row.source.accountingUnitNet)) {
    row.quantity = +toFixed(nVal * row.source.accountingUnitNet, row.measurePrecision)
  } else {
    row.quantity = undefined
  }
}

// 使用草稿/修改时，为数据设置监听
function setFormCallback(form) {
  form.list = form.list.map((v) => reactive(v))
  const trigger = watch(
    [tableRef, headerRef],
    ([tRef, hRef]) => {
      if (tRef && hRef) {
        // 将相同的材料设置为同一个对象，便于计算
        extractSource(form.list)
        initCheckOverMaxWeight(form.list)
        // 初始化选中数据
        form.list.forEach((row) => {
          rowWatch(row)
          checkOverSource(row)
        })
        nextTick(() => {
          trigger() // 执行一次后取消当前监听
        })
      }
    },
    { immediate: true, deep: true }
  )
  fixMaxHeight()
}
</script>

<style lang="scss" scoped>
.el-table {
  ::v-deep(.current-row > td.el-table__cell) {
    --el-table-current-row-background-color: #d7ffef;
  }
}
</style>

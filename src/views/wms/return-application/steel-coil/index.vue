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
          </template>
        </el-expand-table-column>
        <!-- 基础信息 -->
        <material-base-info-columns :basic-class="basicClass" field="source" fixed="left" show-project />
        <!-- 次要信息 -->
        <material-secondary-info-columns :basic-class="basicClass" field="source" fixed="left" />
        <!-- <el-table-column prop="length" align="center" width="110px" :label="`长 (${baseUnit.length.unit})`">
          <template #default="{ row }">
            <common-input-number
              v-model="row.length"
              :max="+row.source.length"
              :controls="false"
              :min="0"
              :precision="baseUnit.length.precision"
              size="mini"
              placeholder="长"
            />
          </template>
        </el-table-column> -->
        <el-table-column key="mete" prop="mete" align="center" :label="`重量 (${baseUnit.weight.unit})`" width="120px">
          <template #default="{ row }">
            <common-input-number
              v-model="row.mete"
              :min="0"
              :max="+row.source.singleReturnableMete"
              controls-position="right"
              :controls="false"
              :precision="baseUnit.weight.precision"
              size="mini"
              placeholder="重量"
            />
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
import { steelCoilReturnApplication } from '@/api/wms/return/application'
import { edit as editReturnApplication } from '@/api/wms/return/raw-mat-application-record'

import { ref, watch, defineEmits, defineProps, reactive, nextTick } from 'vue'
import { rawMatClsEnum } from '@/utils/enum/modules/classification'
import { calcSteelCoilLength } from '@/utils/wms/measurement-calc'
import { toFixed } from '@/utils/data-type'

import useMaxHeight from '@compos/use-max-height'
import useMatBaseUnit from '@/composables/store/use-mat-base-unit'
import useForm from '@/composables/form/use-form'
import MaterialBaseInfoColumns from '@/components-system/wms/table-custom-field-columns/material-base-info-columns/index.vue'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-custom-field-columns/material-secondary-info-columns/index.vue'
import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import WarehouseSetColumns from '../components/warehouse-set-columns.vue'
import CommonHeader from '../components/common-header.vue'
import useCurrentRow from '../composables/use-current-row'
import useFormSet from '../composables/use-form-set'
import useCommonCalc from '../composables/use-common-calc'

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
const permission = ['wms_steelCoilReturnApplication:submit']
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
// 钢板类型
const basicClass = rawMatClsEnum.STEEL_COIL.V
// 当前分类基础单位
const { baseUnit } = useMatBaseUnit(basicClass)

const tableRules = {
  id: [{ required: true, message: '请选择退库物料', trigger: 'change' }],
  mete: [{ required: true, message: '请填写重量', trigger: 'blur' }],
  factoryId: [{ required: true, message: '请选择工厂', trigger: 'change' }],
  warehouseId: [{ required: true, message: '请选择存储位置', trigger: 'change' }]
}

const { cu, form, FORM } = useForm(
  {
    title: '钢卷退库',
    formStore: !props.edit,
    formStoreKey: 'WMS_RETURN_APPLICATION_STEEL_COIL',
    permission: permission,
    defaultForm: defaultForm,
    useDraftCallback: setFormCallback,
    clearDraftCallback: init,
    api: props.edit ? editReturnApplication : steelCoilReturnApplication
  },
  formRef,
  props.detail
)

// 通用计算校验
const { extractSource, checkOverSource, initCheckOverMaxWeight } = useCommonCalc({ form })

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
    headerRef.value && headerRef.value.calcAllQuantity()
  })
  // 计算理论及单重
  watch([() => row.mete, baseUnit], () => {
    console.log(2333)
    calcTheoryLength(row)
    headerRef.value && headerRef.value.calcAllWeight()
  })
  watch(
    () => row.mete,
    () => {
      checkOverSource(row)
    },
    { immediate: true }
  )
}

// 计算单件理论重量
async function calcTheoryLength(row) {
  row.theoryLength = await calcSteelCoilLength({
    weight: row.mete,
    width: row.source.width,
    thickness: row.source.thickness
  })
  console.log('theoryLength', row.theoryLength, row.source.theoryLength, row.source.quantity)
  if (row.theoryLength) {
    console.log('aaa', +toFixed((row.theoryLength / row.source.theoryLength) * row.source.quantity))
    row.singleLength = +toFixed((row.theoryLength / row.source.theoryLength) * row.source.quantity)
  } else {
    row.singleLength = undefined
  }
  row.quantity = row.singleLength
}

// 使用草稿/修改时，为数据设置监听
function setFormCallback(form) {
  form.list = form.list.map((v) => reactive(v))
  const trigger = watch(
    [tableRef, headerRef, baseUnit],
    ([tRef, hRef, bu]) => {
      if (tRef && hRef && bu) {
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
          headerRef.value.calcAllQuantity()
          headerRef.value.calcAllWeight()
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

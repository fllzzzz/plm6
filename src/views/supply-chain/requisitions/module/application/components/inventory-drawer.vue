<template>
  <common-drawer ref="drawerRef" title="库存一览" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="90%">
    <template #titleRight> </template>
    <template #content>
      <el-form class="requisitions-inventory-header" style="display: flex">
        <el-form-item label="库存">
          <common-radio
            v-model="query.warehouseType"
            :options="projectWarehouseTypeEnum.ENUM"
            type="enum"
            size="small"
            @change="fetchList"
          />
        </el-form-item>
        <el-form-item style="margin-left: 80px" label="种类" v-if="params.basicClass & steelClsEnum.STEEL_PLATE.V">
          <common-radio
            v-model="query.basicClass"
            :options="steelClsEnum.ENUM"
            :unshowVal="[steelClsEnum.SECTION_STEEL.V]"
            type="enum"
            size="small"
            @change="fetchList"
          />
        </el-form-item>
      </el-form>
      <common-table v-loading="tableLoading" :data="tableData" :cell-style="handleCellStyle" :max-height="maxHeight" style="width: 100%">
        <material-base-info-columns :basic-class="basicClass" specMerge>
          <template #snTag="{ row }">
            <table-cell-tag
              v-if="!row?.project?.id && query.warehouseType === projectWarehouseTypeEnum.PROJECT.V"
              name="公共库"
              :offset="15"
            />
          </template>
        </material-base-info-columns>
        <el-table-column label="可利用数量" prop="canUseQuantity" align="right" width="100">
          <template #default="{ row }">
            <span>{{ row.canUseQuantity }}</span>
          </template>
        </el-table-column>
        <!-- 单位及其数量 -->
        <material-unit-quantity-columns :basic-class="basicClass" />
        <!-- 次要信息 -->
        <material-secondary-info-columns :basic-class="basicClass" />
        <!-- 仓库信息 -->
        <warehouse-info-columns
          show-workshop
          :show-project="showProjectInfo"
          :show-monomer="showProjectInfo"
          :show-area="showProjectInfo"
        />
        <el-table-column label="操作" width="70" align="center" fixed="right">
          <template #default="{ row: { sourceRow: row } }">
            <common-button icon="el-icon-plus" :disabled="!row.canUseQuantity" type="success" size="mini" @click="add(row)" />
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
  <common-dialog v-model="quantityVisible" title="选择数量" :width="460">
    <template #titleRight>
      <common-button size="mini" type="primary" @click="confirmIt">确定</common-button>
    </template>
    <div class="tip">
      <span>* 提示：</span>
      <span> 若选择此材料，对应物料库存在申购提交之后将被冻结！</span>
    </div>
    <el-form label-width="110px">
      <el-form-item label="物料种类">
        <span>{{ operateInfo.classifyName }}</span>
      </el-form-item>
      <el-form-item label="规格">
        <span>{{ operateInfo.specMerge }}</span>
      </el-form-item>
      <el-form-item label="可利用数量">
        <span>{{ operateInfo.canUseQuantity }}</span>
      </el-form-item>
      <el-form-item
        :label="`利用数量(${basicClass & rawMatClsEnum.MATERIAL.V ? operateInfo?.outboundUnit : baseUnit[basicClass]?.measure?.unit})`"
        required
      >
        <common-input-number
          v-model="quantity"
          :min="1"
          :max="operateInfo.canUseQuantity"
          controls-position="right"
          :controls="false"
          :step="1"
          :precision="basicClass & rawMatClsEnum.MATERIAL.V ? operateInfo?.outboundUnitPrecision : baseUnit[basicClass]?.measure?.precision"
          size="mini"
          placeholder="数量"
        />
      </el-form-item>
    </el-form>
  </common-dialog>
</template>

<script setup>
import { inventoryGet } from '@/api/supply-chain/requisitions-manage/requisitions'
import { defineProps, defineEmits, ref, computed, inject } from 'vue'

import { steelClsEnum, rawMatClsEnum } from '@enum-ms/classification'
import { projectWarehouseTypeEnum } from '@enum-ms/wms'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { isBlank, isNotBlank } from '@data-type/index'

import useMatBaseUnit from '@/composables/store/use-mat-base-unit'
import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import MaterialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import MaterialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import WarehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import { ElMessage } from 'element-plus'

const drawerRef = ref()
const emit = defineEmits(['update:visible', 'use-inventory'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  params: {
    type: Object,
    default: () => {}
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook })

// 高度
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header', '.requisitions-inventory-header'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

const defaultQuery = {
  warehouseType: projectWarehouseTypeEnum.PUBLIC.V
}
const query = ref({})
const quantityVisible = ref(false)
const quantity = ref()
const operateInfo = ref({})
const tableLoading = ref(false)
const tableData = ref([])

const useInventoryInfo = inject('useInventoryInfo')

// 当前物料种类
const basicClass = computed(() => query.value.basicClass)

const { baseUnit } = useMatBaseUnit() // 当前分类基础单位

function showHook() {
  query.value = defaultQuery
  query.value.basicClass = props.params?.basicClass
  fetchList()
}

async function fetchList() {
  try {
    tableLoading.value = true
    const { classifyId, specification, basicClass, thickness, width, length, materialInventoryId, quantity } = props.params
    const _query = { spec: specification, classifyId, thickness, width, length, ...query.value }
    // 钢板查询钢卷时 去掉classifyId
    if (basicClass !== query.value.basicClass) {
      _query.classifyId = undefined
    }
    const { content } = await inventoryGet(_query)
    content.forEach((v) => {
      let _frozenQuantity = v.frozenQuantity || 0
      if (useInventoryInfo.value?.[v.id]) _frozenQuantity += useInventoryInfo.value[v.id]
      if (materialInventoryId && materialInventoryId === v.id) _frozenQuantity -= quantity || 0
      v.canUseQuantity = v.quantity - _frozenQuantity
    })
    await setSpecInfoToList(content)
    tableData.value = await numFmtByBasicClass(content, {
      toSmallest: false,
      toNum: true
    })
  } catch (er) {
    console.log('获取可申购库存失败', er)
  } finally {
    tableLoading.value = false
  }
}

const showProjectInfo = computed(() => {
  // 是否显示项目相关信息
  return query.value?.projectWarehouseType === projectWarehouseTypeEnum.PROJECT.V
})

function add(row) {
  operateInfo.value = row
  quantity.value = undefined
  quantityVisible.value = true
}

function confirmIt() {
  if (isBlank(quantity.value)) {
    ElMessage.warning('请选择利用数量')
    return
  }
  emit('use-inventory', quantity.value, operateInfo.value)
  quantityVisible.value = false
  handleClose()
}

function handleCellStyle({ row, column, rowIndex, columnIndex }) {
  if (isFullMatch(row)) {
    return 'background-color:#f5ffef;'
  }
}

function isFullMatch(row) {
  let match = true
  const { classifyId, thickness, width, length } = props.params
  if (isNotBlank(classifyId) && row.classifyId !== classifyId) {
    match = false
  }
  // 钢板
  if (row.basicClass & rawMatClsEnum.STEEL_PLATE.V) {
    if (isNotBlank(thickness) && row.thickness !== thickness) {
      match = false
    }
    if (isNotBlank(width) && row.width !== width) {
      match = false
    }
    if (isNotBlank(length) && row.length !== length) {
      match = false
    }
  }
  // 型材
  if (row.basicClass & rawMatClsEnum.SECTION_STEEL.V) {
    if (isNotBlank(length) && row.length !== length) {
      match = false
    }
  }
  return match
}
</script>

<style scoped>
.tip {
  display: inline-block;
  color: red;
  text-decoration: underline;
  margin-bottom: 10px;
}
</style>

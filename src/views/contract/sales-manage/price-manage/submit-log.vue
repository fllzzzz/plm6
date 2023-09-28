<template>
  <common-drawer
    append-to-body
    ref="drawerRef"
    v-model="visible"
    top="10vh"
    width="600px"
    :before-close="handleClose"
    title="保存记录"
    :wrapper-closable="false"
    size="80%"
    custom-class="log-form"
  >
    <template #titleRight>
      <common-button type="primary" size="small" :disabled="tableSelection.length===0" @click="previewVisible=true">确认提交</common-button>
      <common-button type="danger" size="small" :disabled="tableSelection.length===0" @click="deleteItem(selectionIds)">清空</common-button>
    </template>
    <template #content>
      <common-table
        v-if="visible"
        v-loading="tableLoading"
        ref="detailRef"
        border
        :data="list"
        :max-height="maxHeight-120"
        style="width: 100%;"
        class="table-form"
        @selection-change="handleSelectionChange"
      >
        <el-table-column type="selection" width="55" align="center" />
        <template v-if="type===contractSaleTypeEnum.STRUCTURE.V || type===contractSaleTypeEnum.MACHINE_PART.V">
          <el-table-column key="name" prop="name" show-overflow-tooltip label="名称" align="center" min-width="140" />
          <el-table-column key="specification" prop="specification" show-overflow-tooltip label="规格" align="center" min-width="140" />
          <el-table-column key="material" prop="material" show-overflow-tooltip label="材质" align="center" min-width="120" />
          <el-table-column key="totalQuantity" prop="totalQuantity" label="数量" align="center" min-width="70" show-overflow-tooltip />
          <el-table-column key="totalLength" prop="totalLength" label="总长度(米)" align="center" min-width="70" show-overflow-tooltip />
          <el-table-column key="totalWeight" prop="totalWeight" show-overflow-tooltip label="总量(t)" align="center" min-width="120" />
        </template>
        <template v-if="type===contractSaleTypeEnum.ENCLOSURE.V">
          <el-table-column key="name" prop="name" show-overflow-tooltip label="名称" align="center" min-width="120" />
          <el-table-column v-if="props.category !== TechnologyTypeAllEnum.BENDING.V" key="plate" prop="plate" show-overflow-tooltip label="板型" align="center" width="100" />
          <el-table-column key="thickness" prop="thickness" show-overflow-tooltip label="厚度(mm)" align="center">
            <template #default="{ row }">
              <span>{{row.thickness?row.thickness.toFixed(DP.MES_ENCLOSURE_T__MM):''}}</span>
            </template>
          </el-table-column>
          <el-table-column v-if="props.category === TechnologyTypeAllEnum.BENDING.V" key="unfoldedWidth" prop="unfoldedWidth" show-overflow-tooltip label="展开宽度(mm)" align="center">
            <template #default="{ row }">
              <span>{{row.unfoldedWidth?row.unfoldedWidth.toFixed(DP.MES_ENCLOSURE_W__MM):''}}</span>
            </template>
          </el-table-column>
          <el-table-column v-if="props.category === TechnologyTypeAllEnum.BENDING.V" key="bendTimes" prop="bendTimes" show-overflow-tooltip label="折弯次数" align="center" />
          <el-table-column key="brand" prop="brand" show-overflow-tooltip label="品牌" align="center" />
          <el-table-column key="color" prop="color" show-overflow-tooltip label="颜色" align="center" width="100" />
          <el-table-column key="totalQuantity" prop="totalQuantity" :show-overflow-tooltip="true" label="数量(张)" align="center" width="100" />
          <el-table-column key="totalArea" prop="totalArea" show-overflow-tooltip label="总面积(㎡)" align="center" />
          <el-table-column key="totalLength" prop="totalLength" show-overflow-tooltip label="总长度(m)" align="center" />
          <el-table-column key="category" prop="category" show-overflow-tooltip label="类别" align="center">
            <template #default="{ row }">
              <span>{{ TechnologyTypeAllEnum.V?.[row.category]?.L }}</span>
            </template>
          </el-table-column>
        </template>
        <template v-if="type===contractSaleTypeEnum.AUXILIARY_MATERIAL.V">
          <el-table-column prop="useProperty" label="使用类别" align="center">
            <template #default="{ row }">
              <span>{{ row.useProperty ? auxiliaryMaterialUseTypeEnum.VL[row.useProperty] : '-' }}</span>
            </template>
          </el-table-column>
          <el-table-column prop="name" label="名称" align="center" show-overflow-tooltip />
          <el-table-column show-overflow-tooltip prop="specification" label="规格" align="center" min-width="120" />
          <el-table-column show-overflow-tooltip prop="measureUnit" label="单位" align="center" />
          <el-table-column show-overflow-tooltip prop="quantity" label="数量" align="center" />
          <el-table-column prop="accountingUnit" show-overflow-tooltip label="核算单位" align="center" min-width="100" />
          <el-table-column prop="mete" show-overflow-tooltip label="核算量" align="center" min-width="100" />
        </template>
        <el-table-column key="pricingManner" prop="pricingManner" show-overflow-tooltip label="计价方式" align="center" min-width="120" v-if="type!==contractSaleTypeEnum.AUXILIARY_MATERIAL.V">
          <template #default="{ row }">
            <span style="color:red;">{{ pricingMannerEnum.VL[row.pricingManner] }}</span>
          </template>
        </el-table-column>
        <el-table-column key="unitPrice" prop="unitPrice" show-overflow-tooltip label="综合单价" align="center" min-width="120">
          <template #default="{ row }">
            <span style="color:red;">{{ row.unitPrice!=='同上'?toThousand(row.unitPrice):'-' }}</span>
          </template>
        </el-table-column>
        <el-table-column key="totalPrice" prop="totalPrice" align="center" min-width="120" label="金额">
          <template #default="{ row }">
            <span style="color:red;" v-thousand="{val:row.totalPrice ||0, dp:decimalPrecision.contract}" />
          </template>
        </el-table-column>
        <el-table-column label="操作" width="100px" align="center" fixed="right">
          <template #default="{ row }">
            <common-button icon="el-icon-delete" type="danger" size="mini" v-loading="deleteLoading" @click.stop="deleteItem([row.id])" />
          </template>
        </el-table-column>
      </common-table>
      <!--分页组件-->
      <el-pagination
        :total="total"
        :current-page="queryPage.pageNumber"
        :page-size="queryPage.pageSize"
        style="margin-top: 8px"
        layout="total, prev, pager, next, sizes"
        @size-change="handleSizeChange"
        @current-change="handleCurrentChange"
      />
      <mPreview v-model="previewVisible" :modified-data="tableSelection" :params="previewParams" @success="handleSuccess" :showType="'log'"/>
    </template>
  </common-drawer>
</template>

<script setup>
import { getTempPrice, deleteTempPrice } from '@/api/contract/sales-manage/price-manage/common'
import { defineProps, defineEmits, ref, watch, computed } from 'vue'
import { ElMessage } from 'element-plus'
import useVisible from '@compos/use-visible'

import { auxiliaryMaterialUseTypeEnum } from '@enum-ms/plan'
import { contractSaleTypeEnum } from '@enum-ms/mes'
import { convertUnits } from '@/utils/convert/unit'
import { DP } from '@/settings/config'
import { toThousand } from '@/utils/data-type/number'
import { TechnologyTypeAllEnum, pricingMannerEnum, enclosureSettlementTypeEnum, standardPartPriceSearchEnum } from '@enum-ms/contract'
import useDecimalPrecision from '@compos/store/use-decimal-precision'
import useMaxHeight from '@compos/use-max-height'

import usePagination from '@compos/use-pagination'
import mPreview from './preview'

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  },
  areaId: {
    type: [Number, String],
    default: undefined
  },
  category: {
    type: [Number, String],
    default: undefined
  },
  enclosurePlanId: {
    type: [Number, String],
    default: undefined
  },
  monomerId: {
    type: [Number, String],
    default: undefined
  },
  projectId: {
    type: [Number, String],
    default: undefined
  },
  projectType: {
    type: [Number, String],
    default: undefined
  },
  type: {
    type: [Number, String],
    default: undefined
  },
  relationType: {
    type: [Number, String],
    default: undefined
  }
})

const { decimalPrecision } = useDecimalPrecision()
const tableLoading = ref(false)
const list = ref([])
const detailRef = ref()
const deleteLoading = ref(false)
const previewVisible = ref(false)

const tableSelection = ref([])
const selectionIds = ref([])
const emit = defineEmits(['success', 'refreshCount', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

const { handleSizeChange, handleCurrentChange, total, setTotalPage, queryPage } = usePagination({ fetchHook: fetchList })

watch(
  () => visible.value,
  (val) => {
    if (val) {
      tableSelection.value = []
      selectionIds.value = []
      fetchList()
    }
  },
  { deep: true, immediate: true }
)
const drawerRef = ref()

// 预览参数
const previewParams = computed(() => {
  switch (props.type) {
    case contractSaleTypeEnum.STRUCTURE.V:
    case contractSaleTypeEnum.MACHINE_PART.V:
      return (
        {
          monomerId: props.monomerId,
          areaId: props.areaId,
          type: props.type
        }
      )
    case contractSaleTypeEnum.ENCLOSURE.V:
      return (
        {
          enclosurePlanId: props.enclosurePlanId,
          type: props.type
        }
      )
    case contractSaleTypeEnum.AUXILIARY_MATERIAL.V:
      return (
        {
          category: props.category,
          enclosurePlanId: props.enclosurePlanId,
          relationType: props.relationType,
          areaId: props.areaId,
          monomerId: props.monomerId,
          type: props.type
        }
      )
    default:
      return {}
  }
})

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.log-form',
    extraBox: '.el-drawer__header',
    wrapperBox: '.el-drawer__body',
    paginate: false,
    minHeight: 300,
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

async function deleteItem(data) {
  if (!data.length) {
    ElMessage.error('请先选择明细')
    return
  }
  try {
    deleteLoading.value = true
    await deleteTempPrice(data)
    ElMessage.success('删除成功')
    fetchList()
    handleSuccess()
    emit('success')
  } catch (error) {
    console.log('价格暂存记录删除', error)
  } finally {
    deleteLoading.value = false
  }
}

function handleSuccess() {
  fetchList()
  emit('refreshCount')
}

function handleSelectionChange(val) {
  tableSelection.value = val
  selectionIds.value = tableSelection.value.map(v => v.id)
}

async function fetchList() {
  const _list = []
  if (!props.projectId || !props.type) {
    list.value = _list
    return
  }
  tableLoading.value = true
  try {
    let params = {}
    switch (props.type) {
      case contractSaleTypeEnum.MACHINE_PART.V:
        params = {
          areaId: props.areaId,
          monomerId: props.monomerId,
          projectId: props.projectId,
          projectType: props.projectType,
          type: props.type,
          ...queryPage
        }
        break
      case contractSaleTypeEnum.ENCLOSURE.V:
        params = {
          category: props.category,
          enclosurePlanId: props.enclosurePlanId,
          projectId: props.projectId,
          projectType: props.projectType,
          type: props.type,
          ...queryPage
        }
        break
      case contractSaleTypeEnum.AUXILIARY_MATERIAL.V:
        params = props.relationType === standardPartPriceSearchEnum.STRUCTURE.V ? {
          areaId: props.areaId,
          monomerId: props.monomerId,
          projectId: props.projectId,
          projectType: props.projectType,
          relationType: props.relationType,
          type: props.type,
          ...queryPage
        } : {
          category: props.category,
          enclosurePlanId: props.enclosurePlanId,
          projectId: props.projectId,
          projectType: props.projectType,
          relationType: props.relationType,
          type: props.type,
          ...queryPage
        }
        break
      default:
        params = {
          areaId: props.areaId,
          monomerId: props.monomerId,
          projectId: props.projectId,
          projectType: props.projectType,
          type: props.type,
          ...queryPage
        }
        break
    }
    const { content = [], totalElements } = await getTempPrice(params)
    content.map(v => {
      let filerData = {}
      switch (props.type) {
        case contractSaleTypeEnum.MACHINE_PART.V:
          filerData = v.machinePart
          break
        case contractSaleTypeEnum.ENCLOSURE.V:
          filerData = v.enclosure
          break
        case contractSaleTypeEnum.AUXILIARY_MATERIAL.V:
          filerData = v.standardPart
          break
        default:
          filerData = v.artifact
          break
      }
      if (props.type !== contractSaleTypeEnum.AUXILIARY_MATERIAL.V) {
        filerData.originPricingManner = filerData.pricingManner
      }
      filerData.originUnitPrice = filerData.unitPrice
      const obj = {
        ...filerData,
        ...v
      }
      if (props.type === contractSaleTypeEnum.MACHINE_PART.V || props.type === contractSaleTypeEnum.STRUCTURE.V) {
        obj.totalWeight = convertUnits(obj.totalWeight, 'kg', 't', DP.COM_WT__T)
        obj.totalPrice = (obj.pricingManner === pricingMannerEnum.WEIGHT.V ? obj.totalWeight : obj.totalLength) * (obj.unitPrice && typeof obj.unitPrice === 'number' ? obj.unitPrice : 0)
      } else if (props.type === contractSaleTypeEnum.ENCLOSURE.V) {
        obj.totalPrice = (obj.pricingManner === enclosureSettlementTypeEnum.LENGTH.V ? obj.totalLength : obj.totalArea) * (obj.unitPrice && typeof obj.unitPrice === 'number' ? obj.unitPrice : 0)
      } else {
        obj.totalPrice = obj.mete * (obj.unitPrice && typeof obj.unitPrice === 'number' ? obj.unitPrice : 0)
      }
      _list.push(obj)
    })
    setTotalPage(totalElements)
  } catch (error) {
    console.log('价格暂存记录', error)
  } finally {
    list.value = _list
    tableLoading.value = false
  }
}

</script>
<style lang="scss" scoped>
::v-deep(.el-input-number .el-input__inner) {
  text-align: left;
}
::v-deep(.content-class){
  width:25% !important;
}
::v-deep(.desc-label){
  width:149px !important;
}
.project-div{
  word-break: break-all;
  overflow: hidden;
  text-overflow: ellipsis;
  display: -webkit-box;
  line-clamp: 2;
  -webkit-box-orient: vertical;
  -webkit-line-clamp: 2;
}
</style>

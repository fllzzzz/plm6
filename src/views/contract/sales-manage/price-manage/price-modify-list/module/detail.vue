<template>
  <common-dialog
    title="变更详情"
    append-to-body
    :visible="crud.detailVisible"
    width="1000px"
    :before-close="crud.cancelDetail"
    show-close
    top="8vh"
  >
    <template #titleRight>
      <span v-if="checkPermission(crud.permission.audit) && props.detailInfo.status === reviewStatusEnum.UNREVIEWED.V">
        <common-button size="mini" type="success" @click.stop="submit(reviewStatusEnum.PASS.V)">同 意</common-button>
        <common-button size="mini" type="danger" @click.stop="submit(reviewStatusEnum.REFUSE.V)">拒 绝</common-button>
      </span>
    </template>
    <common-table :data="list" :data-format="dataFormat" :max-height="maxHeight-180">
      <el-table-column label="序号" type="index" align="center" width="60" />
      <template v-if="props.detailInfo.type === contractSaleTypeEnum.STRUCTURE.V">
        <el-table-column :show-overflow-tooltip="true" prop="name" label="结构名称" align="center" />
        <el-table-column prop="material" label="材质" align="center" />
        <el-table-column key="specification" prop="specification" show-overflow-tooltip label="规格" align="center" />
        <el-table-column key="totalQuantity" prop="totalQuantity" label="数量" align="center" show-overflow-tooltip />
        <el-table-column align="center" prop="pricingManner" label="计价方式">
          <template #default="{ row }">
            <cell-change-preview
              :old="pricingMannerEnum.VL[row.oldPricingManner]"
              :new="pricingMannerEnum.VL[row.newPricingManner]"
              v-if="row.oldPricingManner !== row.newPricingManner"
            />
            <span v-else>{{ pricingMannerEnum.VL[row.oldPricingManner] }}</span>
          </template>
        </el-table-column>
      </template>
      <template v-if="props.detailInfo.type === contractSaleTypeEnum.ENCLOSURE.V">
        <el-table-column :show-overflow-tooltip="true" prop="name" label="名称" align="center" />
        <el-table-column key="specification" prop="specification" show-overflow-tooltip label="规格" align="center" />
        <el-table-column v-if="list[0]?.category !== mesEnclosureTypeEnum.FOLDING_PIECE.V" prop="plate" label="板型" align="center" />
        <el-table-column key="totalQuantity" prop="totalQuantity" label="数量" align="center" show-overflow-tooltip />
        <el-table-column align="center" prop="pricingManner" label="计价方式">
          <template #default="{ row }">
            <cell-change-preview
              :old="enclosureSettlementTypeEnum.VL[row.oldPricingManner]"
              :new="enclosureSettlementTypeEnum.VL[row.newPricingManner]"
              v-if="row.oldPricingManner !== row.newPricingManner"
            />
            <span v-else>{{ enclosureSettlementTypeEnum.VL[row.oldPricingManner] }}</span>
          </template>
        </el-table-column>
      </template>
      <template v-if="props.detailInfo.type === contractSaleTypeEnum.AUXILIARY_MATERIAL.V">
        <el-table-column prop="name" label="配套件名称" align="center" />
        <el-table-column prop="specification" label="规格" />
        <el-table-column key="totalQuantity" prop="totalQuantity" label="数量" align="center" show-overflow-tooltip />
      </template>
      <el-table-column align="center" prop="price" label="综合单价">
        <template #default="{ row }">
          <template v-if="props.detailInfo.type === contractSaleTypeEnum.STRUCTURE.V">
            <span v-if="row.oldUnitPrice === row.newUnitPrice">{{ row.oldUnitPrice }}</span>
            <cell-change-preview :old="row.oldUnitPrice" :new="row.newUnitPrice" v-else />
          </template>
          <cell-change-preview :old="row.oldUnitPrice" :new="row.newUnitPrice" v-else />
        </template>
      </el-table-column>
    </common-table>
  </common-dialog>
</template>

<script setup>
import { priceModifySave as save } from '@/api/contract/sales-manage/price-manage/common'
import { ref, defineProps, defineEmits, computed } from 'vue'

import checkPermission from '@/utils/system/check-permission'
import { contractSaleTypeEnum, mesEnclosureTypeEnum } from '@enum-ms/mes'
import { reviewStatusEnum } from '@enum-ms/common'
import { pricingMannerEnum, enclosureSettlementTypeEnum } from '@enum-ms/contract'

import { regDetail } from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import cellChangePreview from '@comp-common/cell-change-preview'
import useDecimalPrecision from '@compos/store/use-decimal-precision'

const { decimalPrecision } = useDecimalPrecision()

const emit = defineEmits(['success'])

const props = defineProps({
  detailInfo: {
    type: Object,
    default: () => {}
  }
})

const { maxHeight } = useMaxHeight({ extraBox: '.el-drawer__header', wrapperBox: '', extraHeight: 4 })

const list = ref([])
const dataFormat = computed(() => {
  return [
    ['oldUnitPrice', ['to-thousand', (decimalPrecision.value.contract === 2 ? 3 : decimalPrecision.value.contract)]],
    ['newUnitPrice', ['to-thousand', (decimalPrecision.value.contract === 2 ? 3 : decimalPrecision.value.contract)]]
  ]
})
const { crud, detail, CRUD } = regDetail()

// 详情加载后
CRUD.HOOK.beforeDetailLoaded = async (crud) => {
  list.value = []
  try {
    list.value = detail.content || []
  } catch (error) {
    crud.notify('获取构件详情', CRUD.NOTIFICATION_TYPE.ERROR)
  }
}

// 审核
async function submit(status) {
  try {
    await save({
      id: props.detailInfo.id,
      status
    })
    crud.notify('审核成功', CRUD.NOTIFICATION_TYPE.SUCCESS)
    crud.cancelDetail()
  } catch (error) {
    console.log('商务审核失败', error)
  } finally {
    emit('success')
  }
}
</script>

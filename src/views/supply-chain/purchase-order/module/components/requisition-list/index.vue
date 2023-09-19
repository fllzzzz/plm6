<template>
  <div class="requisition-main-content">
    <div class="content-left">
      <requisition-order ref="requisitionOrderRef" />
    </div>
    <div class="vertical-dashed-divider" />
    <div class="content-right">
      <div class="right-head flex-rbs">
        <!-- 关联申购单-->
        <span class="requisitions-list">
          <span class="label">申购清单</span>
          <el-tag v-for="item in form.requisitions" type="success" :key="item.id" effect="plain" class="requisitions-sn-tag">
            {{ item.serialNumber }}
          </el-tag>
        </span>
      </div>
      <component v-loading="loading" ref="compRef" :is="currentView" @add-purchase="addPurchase" />
    </div>
  </div>
</template>

<script setup>
import { canPurchaseDetail } from '@/api/supply-chain/requisitions-manage/requisitions'
import requisitionOrder from './module/requisition-order'
import { defineExpose, defineEmits, computed, ref, watch, inject, nextTick } from 'vue'

import { matClsEnum } from '@/utils/enum/modules/classification'
import { STEEL_ENUM } from '@/settings/config'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { compareArray } from '@/utils/data-type/array'
import { calcSteelPlateWeight, calcSectionSteelWeight, calcSectionSteelTotalLength } from '@/utils/wms/measurement-calc'
import { materialPurchaseClsEnum } from '@enum-ms/classification'

import Steel from './module/steel/index.vue'
import AuxMat from './module/auxiliary-material/index.vue'
import Manufactured from './module/manufactured/index.vue'

const emit = defineEmits('add-purchase')
const form = inject('cu')?.form

const requisitionOrderRef = ref()
const compRef = ref()
const loading = ref(false)
const list = ref([])

const requisitionIds = computed(() => form.requisitions?.map((v) => v.id) || [])

watch(
  requisitionIds,
  (val, oldVal) => {
    if (!compareArray(val, oldVal)) {
      fetchList()
    }
  },
  { immediate: true, deep: true }
)

const materialType = computed(() => form?.materialType)

const currentView = computed(() => {
  switch (materialType.value) {
    case materialPurchaseClsEnum.STEEL.V:
      return Steel
    case materialPurchaseClsEnum.MATERIAL.V:
      return AuxMat
    case materialPurchaseClsEnum.OTHER.V:
      return AuxMat
    case materialPurchaseClsEnum.MANUFACTURED.V:
      return Manufactured
    default:
      return ''
  }
})

async function fetchList() {
  list.value = []
  if (!requisitionIds.value?.length) {
    nextTick(() => compRef.value?.initList([]))
    return
  }
  try {
    loading.value = true
    const content = (await canPurchaseDetail({ ids: requisitionIds.value })) || []
    if (materialType.value & materialPurchaseClsEnum.MANUFACTURED.V) {
      list.value = content
    } else {
      await setSpecInfoToList(content)
      list.value = await numFmtByBasicClass(
        content,
        {
          toSmallest: false,
          toNum: true
        },
        {
          mete: ['mete', 'productMete']
        }
      )
      // 计算理论重量、申购单量
      for (let i = 0; i < list.value.length; i++) {
        const v = list.value[i]
        v.purchaseSN = form.requisitionsKV[v.applyPurchaseId]?.serialNumber
        v.productQuantity = v.productQuantity || 0
        v.productMete = v.productMete || 0
        const _mete = v.mete
        const _quantity = v.quantity
        v.originMete = _mete
        v.originQuantity = _quantity
        v.canPurchaseQuantity = _quantity - v.productQuantity > 0 ? _quantity - v.productQuantity : 0
        v.canPurchaseMete = _mete - v.productMete > 0 ? _mete - v.productMete : 0
        v.quantity = v.canPurchaseQuantity
        if (v.basicClass & STEEL_ENUM) {
          // v.purchaseNetMete = v.mete / v.quantity
          v.purchaseTotalWeight = v.mete
          v.weighingTotalWeight = v.canPurchaseMete
          if (v.basicClass & matClsEnum.STEEL_PLATE.V) {
            v.theoryWeight = await calcSteelPlateWeight({
              name: v.classifyFullName, // 名称，用于判断是否为不锈钢，不锈钢与普通钢板密度不同
              length: v.length,
              width: v.width,
              thickness: v.thickness
            })
            v.theoryTotalWeight = v.theoryWeight * v.quantity
          }
          if (v.basicClass & matClsEnum.SECTION_STEEL.V) {
            v.theoryWeight = await calcSectionSteelWeight({
              length: v.length, // 长度
              unitWeight: v.unitWeight // 单位重量
            })
            v.theoryTotalWeight = v.theoryWeight * v.quantity
            v.totalLength = calcSectionSteelTotalLength({
              length: v.length, // 长度
              quantity: v.quantity // 数量
            })
          }
        } else {
          v.mete = v.canPurchaseMete
        }
      }
    }
    nextTick(() => {
      compRef.value?.initList(list.value)
      loading.value = false
    })
  } catch (er) {
    loading.value = false
    console.log(er, '获取可采购列表失败')
  }
}

function addPurchase(row, index) {
  emit('add-purchase', row)
}

// 打开申购单列表
async function openInit() {
  nextTick(() => requisitionOrderRef.value?.fetchList())
  await fetchList()
}

defineExpose({
  openInit
})
</script>

<style lang="scss" scoped>
::-webkit-scrollbar {
  /*滚动条整体样式*/
  width: 4px; /*高宽分别对应横竖滚动条的尺寸*/
  height: 4px;
}

.requisition-main-content {
  display: flex;
  flex-direction: row;
  justify-content: flex-start;
  align-items: flex-start;
  height: 100%;

  .content-left {
    width: 405px;
    flex: none;
    height: 100%;
    overflow: auto;
    box-sizing: border-box;
    padding-right: 20px;
  }

  .vertical-dashed-divider {
    margin: 0 16px 0 1px;
  }

  .content-right {
    width: 100%;
    height: 100%;
    overflow-y: auto;
    overflow-x: hidden;

    .el-table {
      ::v-deep(td > .cell) {
        min-height: 30px;
        line-height: 30px;
      }
    }

    .right-head {
      height: 45px;
      .requisitions-list {
        display: block;
        .label {
          font-weight: bold;
          font-size: 15px;
          margin-right: 10px;
          color: var(--el-text-color-regular);
        }
        .requisitions-sn-tag {
          user-select: none;
          min-width: 150px;
          margin-right: 10px;
          text-align: center;
          cursor: pointer;
        }
      }
    }
  }
}
</style>

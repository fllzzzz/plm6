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
      <component ref="compRef" :is="currentView" @add-purchase="addPurchase" />
    </div>
  </div>
</template>

<script setup>
import { canPurchaseDetail } from '@/api/supply-chain/requisitions-manage/requisitions'
import requisitionOrder from './module/requisition-order'
import { defineExpose, defineEmits, computed, ref, watch, inject, nextTick } from 'vue'
import { isBlank } from '@/utils/data-type'

import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { materialPurchaseClsEnum } from '@enum-ms/classification'

import Steel from './module/steel/index.vue'
import AuxMat from './module/auxiliary-material/index.vue'
import Manufactured from './module/manufactured/index.vue'

const emit = defineEmits('add-purchase')
const form = inject('crud')?.form

const requisitionOrderRef = ref()
const compRef = ref()
const list = ref([])

watch(
  () => form.requisitions,
  () => {
    fetchList()
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
    case materialPurchaseClsEnum.MANUFACTURED.V:
      return Manufactured
    default:
      return ''
  }
})

async function fetchList() {
  list.value = []
  if (!form.requisitions?.length) return
  try {
    const ids = form.requisitions?.map((v) => v.id) || []
    const content = (await canPurchaseDetail({ ids })) || []
    if (materialType.value & materialPurchaseClsEnum.MANUFACTURED.V) {
      list.value = content
    } else {
      await setSpecInfoToList(content)
      list.value = await numFmtByBasicClass(content, {
        toSmallest: false,
        toNum: true
      })
    }
    nextTick(() => compRef.value?.initList(list.value))
  } catch (er) {
    console.log(er, '获取可采购列表失败')
  }
}

function addPurchase(row, index) {
  if (isBlank(form.requisitionListKV?.[row.id])) {
    form.requisitionListKV[row.id] = row
  }
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
.requisition-main-content {
  display: flex;
  flex-direction: row;
  justify-content: flex-start;
  align-items: flex-start;
  height: 100%;

  .content-left {
    width: 400px;
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

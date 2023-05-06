<template>
  <common-drawer
    customClass="amortization-setting-drawer"
    :close-on-click-modal="false"
    append-to-body
    v-model="visible"
    :before-close="handleClose"
    title="摊销设置"
    :wrapper-closable="true"
    size="50%"
  >
    <template #content>
      <div class="flex-r">
        <el-card body-style="padding: 10px" class="amortization-tree">
          <template #header>
            <div>
              <span>材料-摊销分类设置</span>
              <div class="icons">
                <template v-if="materialEdit">
                  <svg-icon class="icon" icon-class="comp-save" style="fill: #ffac00" />
                  <svg-icon class="icon" icon-class="comp-quit" @click="materialEdit = false" />
                </template>
                <el-edit v-else class="icon" @click="materialEdit = true" />
              </div>
            </div>
          </template>
          <el-tree
            :style="{ maxHeight: maxHeight + 'px' }"
            :data="materialTree"
            :props="defaultProps"
            :show-checkbox="materialEdit"
            :expand-on-click-node="false"
            node-key="id"
            highlight-current
            default-expand-all
          />
        </el-card>
        <el-card body-style="padding: 10px" class="indirect-tree">
          <template #header>
            <div>间接费用-摊销分类设置</div>
          </template>
          <el-tree
            :style="{ maxHeight: maxHeight + 'px' }"
            :data="materialTree"
            :props="defaultProps"
            :expand-on-click-node="false"
            node-key="id"
            highlight-current
            default-expand-all
          />
        </el-card>
      </div>
    </template>
  </common-drawer>
</template>

<script setup>
import { subjectTree } from '@/api/contract/expense-entry/gas-cost'
import { amortizationClassTree } from '@/api/contract/expense-entry/amortization-manage'
import { ref, defineEmits, defineProps, watch, inject } from 'vue'

import { matClsEnum } from '@enum-ms/classification'
import { amortizationTypeEnum } from '@enum-ms/contract'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

const props = defineProps({
  modelValue: {
    type: Boolean,
    require: true
  }
})

const amortizationKV = inject('amortizationKV')

const emit = defineEmits(['success', 'update:modelValue'])
const { visible, handleClose } = useVisible({ emit, props })

watch(
  () => visible.value,
  (val) => {
    if (val) {
      getAmortizationTree()
      getMaterialTree()
      console.log('amortizationKV: ', amortizationKV)
    }
  }
)

const amortizationTree = ref([])
const materialTree = ref([])
const materialEdit = ref(false)
const defaultProps = ref({
  children: 'children',
  label: 'name'
})

const { maxHeight } = useMaxHeight({
  navbar: false,
  extraBox: ['.el-drawer__header', '.el-card__header'],
  wrapperBox: ['.el-drawer__body'],
  extraHeight: 70
})

// 获取材料分类的全部数据
async function getMaterialTree() {
  try {
    const material = matClsEnum.MATERIAL.V + matClsEnum.OTHER.V // 材料
    materialTree.value = (await subjectTree({ basicClassEnum: material })) || []
    console.log('materialTree.value: ', materialTree.value)
  } catch (error) {
    console.log('获取材料树失败', error)
  }
}

// 获取摊销种类内已设置的材料分类
async function getAmortizationTree() {
  try {
    amortizationTree.value = (await amortizationClassTree({ basicClassEnum: amortizationTypeEnum.MATERIAL.V })) || []
  } catch (e) {
    console.log('获取摊销种类失败', e)
  }
}
</script>

<style lang="scss" scoped>
::v-deep(.el-card) {
  flex: 1;
  .el-card__header {
    padding: 10px;
    color: #fff;
    .icons {
      float: right;
      font-size: 20px;
      cursor: pointer;
      .icon {
        width: 1em;
        height: 1em;
        margin-left: 6px;
        vertical-align: 0;
      }
    }
  }
  .el-tree {
    overflow-y: auto;
  }
  &.amortization-tree {
    margin-right: 20px;
    .el-card__header {
      background: #0079ff;
    }
  }
  &.indirect-tree {
    .el-card__header {
      background: #36ae81;
    }
  }
}
</style>

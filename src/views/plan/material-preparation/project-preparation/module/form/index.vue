<template>
  <common-drawer
    ref="drawerRef"
    v-bind="$attrs"
    :visible="drawerVisible"
    :before-close="crud.cancelCU"
    :title="title"
    :show-close="true"
    size="100%"
    :close-on-click-modal="false"
    custom-class="project-preparation-form"
  >
    <template #titleAfter>
      <el-tag class="info-tag filter-item" effect="dark" type="warning"> 清单上传人：<span v-split="listUploaderNames" v-empty /> </el-tag>
    </template>
    <template #titleRight>
      <common-button :loading="crud.status.cu === CRUD.STATUS.PROCESSING" size="mini" type="primary" @click="crud.submitCU">
        提 交
      </common-button>
    </template>
    <template #content>
      <div class="main-content">
        <div class="head">
          <list-and-match @add="handleAddInventoryMaterial" @selected-change="handleSelectedChange" />
        </div>
        <!-- <el-divider> -->
        <!-- <span class="title"><span>库存利用清单</span> | 采购清单</span> -->
        <!-- </el-divider> -->
        <div class="middle">
          <div class="middle-head filter-container">
            <div class="filter-left-box">
              <common-radio-button
                class="filter-item"
                v-model="preparationListType"
                :options="preparationListTypeEnum"
                type="enum"
                size="mini"
              />
              <div class="filter-item">
                <el-checkbox v-model="queryFilter.showOnlySelectTechInfo" label="只显示选中的清单的备料信息" size="mini" border />
              </div>
              <template v-if="crud.form.materialBasicClass & STEEL_ENUM">
                <el-tag class="info-tag filter-item" effect="plain" type="info">
                  清单汇总量：
                  <span v-to-fixed="{ val: crud.props.listTotalMete || 0, k: 'COM_WT__KG' }" /> kg
                </el-tag>
                <el-tag class="info-tag filter-item" effect="plain" type="success">
                  库存利用量：
                  <span v-to-fixed="{ val: crud.props.inventoryTotalMete || 0, dp: STEEL_BASE_UNIT.weight.precision }" /> kg
                </el-tag>
                <el-tag class="info-tag filter-item" effect="plain" type="warning">
                  需要采购量：
                  <span v-to-fixed="{ val: crud.props.purchaseTotalMete || 0, dp: STEEL_BASE_UNIT.weight.precision }" /> kg
                </el-tag>
              </template>
            </div>
            <div class="filter-right-box">
              <common-button
                v-if="preparationListType === preparationListTypeEnum.PURCHASE_LIST.V"
                class="filter-item"
                type="success"
                size="mini"
                :disabled="!selectTechnologyRow || crud.props.boolTechEditMode"
                @click="openAddPurchaseMaterialDlg"
              >
                新增材料
              </common-button>
            </div>
          </div>
          <div class="preparation-info flex-rss">
            <div class="preparation-table-list">
              <div class="table-wrapper">
                <el-form ref="formRef" :model="form" size="small" label-position="top" inline label-width="200px">
                  <inventory-table
                    ref="inventoryTableRef"
                    :show="preparationListType === preparationListTypeEnum.INVENTORY_LIST.V"
                    :current-tech-row="selectTechnologyRow"
                    :query-filter="queryFilter"
                    :height="maxHeight"
                  />
                  <purchase-table
                    ref="purchaseTableRef"
                    :show="preparationListType === preparationListTypeEnum.PURCHASE_LIST.V"
                    :current-tech-row="selectTechnologyRow"
                    :query-filter="queryFilter"
                    :height="maxHeight"
                  />
                </el-form>
              </div>
            </div>
            <div class="preparation-remark-info" :style="heightStyle">
              <el-input
                class="remark"
                v-model="form.remark"
                :rows="5"
                type="textarea"
                placeholder="备注"
                maxlength="1000"
                show-word-limit
                style="width: 100%"
              />
              <div class="tip-list">
                <span class="tip">
                  * 备料单保存提交后，“库存利用清单”中相对应的物料，会在物料仓被冻结（被冻结的物料只可以被对应备料单绑定的项目出库）。
                </span>
                <span class="tip">
                  * 冻结的物料被出库，则“库存利用清单”中物料的可利用最小数量为：“当前备料单数量” - (“项目下所有物料单备料冻结总数量 -
                  冻结出库数量”)。
                </span>
              </div>
            </div>
          </div>
        </div>
      </div>
    </template>
  </common-drawer>
  <purchase-material-add-dlg
    v-model="addPurchaseMaterialDlgVisible"
    :technology-row="selectTechnologyRow"
    @success="handleAddPurchaseMaterial"
  />
</template>

<script setup>
import { computed, ref, watch } from 'vue'
import { STEEL_BASE_UNIT, STEEL_ENUM } from '@/settings/config'
import { createUniqueString } from '@/utils/data-type/string'
import { preparationSubmitDTO } from '@/views/plan/material-preparation/dto'

import { regForm } from '@compos/use-crud'
import useMaxHeight from '@/composables/use-max-height'
import ListAndMatch from './list-and-match'
import inventoryTable from './inventory-table.vue'
import purchaseTable from './purchase-table.vue'
import purchaseMaterialAddDlg from './purchase-material-add/dialog.vue'
import { componentTypeEnum } from '@/utils/enum/modules/building-steel'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import cloneDeep from 'lodash/cloneDeep'
import { arr2obj } from '@/utils/convert/type'

// 备料清单选项
const preparationListTypeEnum = {
  INVENTORY_LIST: { L: '库存利用清单', K: 'INVENTORY_LIST', V: 1 },
  PURCHASE_LIST: { L: '需要采购清单', K: 'PURCHASE_LIST', V: 2 }
}
// 当前选择的备料列表类型
const preparationListType = ref(preparationListTypeEnum.INVENTORY_LIST.V)

// drawer标题
const title = ref('备料')
// 清单上传人
const listUploaderNames = ref('')
// 选中的“技术清单汇总”记录
const selectTechnologyRow = ref()
// 表单ref
const formRef = ref()
// 库存利用清单ref
const inventoryTableRef = ref()
// 需要采购清单ref
const purchaseTableRef = ref()
// 采购添加
const addPurchaseMaterialDlgVisible = ref(false)
// crud
const { CRUD, crud, form } = regForm(void 0, formRef)
// drawer 显示
const drawerVisible = computed(() => crud.status.cu > CRUD.STATUS.NORMAL)
// 查询条件
const queryFilter = ref({
  showOnlySelectTechInfo: false
})
// 技术清单 id - info
// const technologyListKV = ref({})

// 高度
const { maxHeight, heightStyle } = useMaxHeight(
  {
    mainBox: '.project-preparation-form',
    extraBox: ['.el-drawer__header', '.head', '.middle-head'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true
  },
  drawerVisible
)

watch(
  () => crud.form.technologyList,
  (list) => {
    if (list && list.length > 0 && list[0].id) {
      crud.props.technologyListKV = arr2obj(list)
    } else {
      crud.props.technologyListKV = {}
    }
  },
  { immediate: true }
)

// 初始化表单
CRUD.HOOK.beforeEditDetailLoaded = async (crud, form) => {
  await handleDetailForTechnologyList(crud, form)
}

CRUD.HOOK.beforeToEdit = (crud, form) => {
  init()
  let range = '' // 备料范围
  if (form.project && form.project.shortName) {
    range += form.project.shortName
  }
  if (form.monomer && form.monomer.name) {
    range = `${range} / ${form.monomer.name}`
  }
  if (form.area && form.area.name) {
    range = `${range} / ${form.area.name}`
  }
  title.value = `备料：${form.serialNumber}（${range}）`
  listUploaderNames.value = form.listUploaderNames
}

CRUD.HOOK.beforeValidateCU = (crud, form) => {
  const invResult = inventoryTableRef.value.validate()
  if (!invResult) return false
  const purResult = purchaseTableRef.value.validate()
  return purResult
}

// 表单提交数据清理
crud.submitFormFormat = async (form) => {
  return await preparationSubmitDTO(form)
}

// 初始化
function init() {
  title.value = '备料'
  selectTechnologyRow.value = undefined
  listUploaderNames.value = ''
  preparationListType.value = preparationListTypeEnum.INVENTORY_LIST.V
  queryFilter.value = {
    showOnlySelectTechInfo: false
  }
  crud.props.boolTechEditMode = false
  crud.props.technologyListKV = {}
  crud.props.techPrepMeteKV = {}
  crud.props.listTotalMete = 0
  crud.props.inventoryTotalMete = 0
  crud.props.purchaseTotalMete = 0
}

// 处理详情的技术清单
async function handleDetailForTechnologyList(crud, form) {
  form.technologyList = form.technologyList || []
  form.sourceTechnologyList = cloneDeep(form.technologyList)
  form.technologyList.forEach((tRow) => {
    tRow.id = tRow.id || createUniqueString() // 唯一编号
    tRow.boundInvIds = [] // 绑定库存利用清单
    tRow.boundPurIds = [] // 绑定需要采购清单
    // 辅材不需要计算汇总量
    if (form.technologyListType !== componentTypeEnum.AUXILIARY_MATERIAL.V) {
      crud.props.listTotalMete += tRow.listMete
    }
  })
  // 辅材，进行数据转换
  if (form.technologyListType === componentTypeEnum.AUXILIARY_MATERIAL.V) {
    await setSpecInfoToList(form.technologyList)
    await numFmtByBasicClass(form.technologyList, {
      toNum: true
    })
  }
}

// 处理“添加”库存利用材料
function handleAddInventoryMaterial(row, technologyRow) {
  inventoryTableRef.value && inventoryTableRef.value.add({ material: row }, technologyRow)
}

// 处理“添加”采购材料
function handleAddPurchaseMaterial(row, technologyRow) {
  purchaseTableRef.value && purchaseTableRef.value.add({ material: row }, technologyRow)
}

// 打开添加采购物料窗口
function openAddPurchaseMaterialDlg() {
  addPurchaseMaterialDlgVisible.value = true
}

// 处理“选中”清单汇总记录
function handleSelectedChange(row) {
  selectTechnologyRow.value = row
}
</script>

<style lang="scss" scoped>
.main-content {
  .head {
    margin-bottom: 10px;
  }
  .middle {
    .middle-head {
      width: calc(100% - 320px);
      height: 39px;
    }
    .preparation-info {
      width: 100%;
      // width: 300px;
      // flex: none;
      // margin-right: 20px;
    }
    .preparation-table-list {
      flex: auto;
      position: relative;
      width: 100%;

      .table-wrapper {
        width: 100%;
        position: absolute;
        right: 0;
        top: 0;
      }
    }
    .preparation-remark-info {
      padding-left: 20px;
      width: 320px;
      flex: none;
      display: flex;
      flex-direction: column;
      .remark {
        ::v-deep(.el-textarea__inner) {
          height: 100%;
        }
        flex: auto;
        height: 100%;
        width: 100%;
        margin-bottom: 10px;
      }
    }
    .tip-list {
      flex: none;
      font-size: 14px;
      color: #e6a23cc2;
      .tip {
        display: block;
      }
      .tip + .tip {
        margin-top: 10px;
      }
    }
  }
}
.info-tag {
  min-width: 170px;
  text-align: center;
}
</style>

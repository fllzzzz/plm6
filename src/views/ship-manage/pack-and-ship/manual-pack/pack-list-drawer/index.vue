<template>
  <common-drawer
    ref="drawerRef"
    title="打包列表"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="75%"
    custom-class="drawer-detail"
  >
    <template #titleRight>
      <!-- <common-button v-permission="permission.pack" type="primary" :loading="packLoading" size="mini" @click="packClick">
        {{ packTypeEnum.VL[packType] }}打包({{ listObj['source' + packTypeEnum.VK[packType]].length }})
      </common-button> -->
      <common-button v-permission="permission.pack" type="primary" :loading="packLoading" size="mini" @click="packClick">
        打包 ({{ totalBadge }})
      </common-button>
    </template>
    <template #content>
      <div class="head-container">
        <!-- <el-radio-group v-model="packType" size="small" class="filter-item">
          <el-radio-button v-for="item in packTypeEnum.ENUM" :key="item.K" :label="item.V" >
            {{ item.L }}({{ listObj['source' + item.K].length }})
          </el-radio-button>
        </el-radio-group> -->
        <component-radio-button
          v-model="packType"
          size="small"
          class="filter-item"
          :options="packTypeEnum.ENUM"
          type="enum"
          :disabledVal="disabledVal"
        >
          <template #suffix="{ item }"> ({{ listObj['source' + item.K].length }})</template>
        </component-radio-button>
        <workshop-select
          v-if="packType !== packTypeEnum.AUXILIARY_MATERIAL.V"
          v-model="workshopId"
          placeholder="请选择车间"
          clearable
          style="width: 200px"
          class="filter-item"
        />
        <!-- <factory-select
          v-if="packType !== packTypeEnum.AUXILIARY_MATERIAL.V"
          v-model="factoryId"
          class="filter-item"
          clearable
          style="width: 200px"
        /> -->
      </div>
      <common-table
        :data="listObj[packTypeEnum.VK[packType]]"
        return-source-data
        :show-empty-symbol="false"
        show-summary
        :summary-method="getSummaries"
        empty-text="暂无数据"
        :max-height="maxHeight"
        style="width: 100%"
        class="manual-pack-list"
        :expand-row-keys="expandRowKeys"
        default-expand-all
        row-key="rowKey"
      >
        <el-table-column type="expand">
          <template #default="{ row }">
            <el-form v-if="row.boolOneCode" style="padding-top: 20px; padding-left: 20px">
              <el-form-item label="一物一码编号：">
                <one-code-number-list
                  v-model="row.numberList"
                  :list="row.originNumberList"
                  :tag-width="100"
                  @change="oneCodeSelectChange(row)"
                ></one-code-number-list>
              </el-form-item>
            </el-form>
          </template>
        </el-table-column>
        <el-table-column label="序号" type="index" align="center" width="60" />
        <template v-if="packType & (packTypeEnum.STRUCTURE.V | packTypeEnum.MACHINE_PART.V)">
          <el-table-column v-if="packType & packTypeEnum.STRUCTURE.V" key="name" prop="name" :show-overflow-tooltip="true" label="名称" width="120px">
            <template v-slot="scope">
              <table-cell-tag v-if="scope.row.workshop" :name="scope.row.workshop?.name" />
              <span>{{ scope.row.name }}</span>
            </template>
          </el-table-column>
          <el-table-column prop="monomer.name" label="单体" align="center" width="100px" />
          <el-table-column prop="area.name" label="区域" align="center" width="100px" />
          <el-table-column prop="serialNumber" label="编号" align="center" width="120px" />
          <el-table-column key="specification" prop="specification" :show-overflow-tooltip="true" label="规格" min-width="140px" />
          <el-table-column key="length" prop="length" :show-overflow-tooltip="true" :label="`长度\n(mm)`" align="left" min-width="85px">
            <template v-slot="scope">
              {{ toFixed(scope.row.length, DP.MES_ARTIFACT_L__MM) }}
            </template>
          </el-table-column>
          <el-table-column key="material" prop="material" :show-overflow-tooltip="true" label="材质" min-width="80px" />
          <el-table-column
            key="netWeight"
            prop="netWeight"
            :show-overflow-tooltip="true"
            :label="`单净重\n(kg)`"
            align="left"
            min-width="80px"
          >
            <template v-slot="scope">
              {{ toFixed(scope.row.netWeight, DP.COM_WT__KG) }}
            </template>
          </el-table-column>
          <el-table-column
            key="grossWeight"
            prop="grossWeight"
            :show-overflow-tooltip="true"
            :label="`单毛重\n(kg)`"
            align="left"
            min-width="80px"
          >
            <template v-slot="scope">
              {{ toFixed(scope.row.grossWeight, DP.COM_WT__KG) }}
            </template>
          </el-table-column>
        </template>

        <!-- <template v-if="packType === packTypeEnum.ENCLOSURE.V">
          <el-table-column key="name" prop="name" :show-overflow-tooltip="true" label="名称" width="120px">
            <template v-slot="scope">
              <table-cell-tag v-if="scope.row.workshop" :name="scope.row.workshop?.name" />
              <span>{{ scope.row.name }}</span>
            </template>
          </el-table-column>
          <el-table-column prop="serialNumber" label="编号" align="center" width="120px" />
          <el-table-column key="plate" prop="plate" :show-overflow-tooltip="true" label="板型" min-width="100px" />
          <el-table-column key="color" prop="color" :show-overflow-tooltip="true" label="颜色" min-width="100px" />
          <el-table-column
            key="thickness"
            prop="thickness"
            :show-overflow-tooltip="true"
            :label="`厚度\n(mm)`"
            align="left"
            min-width="85px"
          >
            <template v-slot="scope">
              {{ toFixed(scope.row.thickness, DP.MES_ENCLOSURE_T__MM) }}
            </template>
          </el-table-column>
          <el-table-column key="width" prop="width" :show-overflow-tooltip="true" :label="`有效宽度\n(mm)`" align="left" min-width="85px">
            <template v-slot="scope">
              {{ toFixed(scope.row.width, DP.MES_ENCLOSURE_W__MM) }}
            </template>
          </el-table-column>
          <el-table-column key="length" prop="length" :show-overflow-tooltip="true" :label="`长度\n(mm)`" align="left" min-width="85px">
            <template v-slot="scope">
              {{ toFixed(scope.row.length, DP.MES_ENCLOSURE_L__MM) }}
            </template>
          </el-table-column>
          <el-table-column
            key="totalArea"
            prop="totalArea"
            :show-overflow-tooltip="true"
            :label="`总面积\n(㎡)`"
            align="left"
            min-width="100px"
          >
            <template v-slot="scope">
              {{ toFixed(scope.row.totalArea, DP.COM_AREA__M2) }}
            </template>
          </el-table-column>
        </template> -->
        <template v-if="packType === packTypeEnum.AUXILIARY_MATERIAL.V">
          <el-table-column key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="编号" min-width="120" />
          <el-table-column key="fullClassName" prop="fullClassName" :show-overflow-tooltip="true" label="辅材类别" min-width="250" />
          <el-table-column key="unit" prop="unit" label="单位" min-width="80px" />
          <el-table-column key="color" prop="color" :show-overflow-tooltip="true" label="颜色" min-width="120" />
          <el-table-column key="specification" prop="specification" :show-overflow-tooltip="true" label="规格" min-width="120" />
        </template>
        <el-table-column key="inQuantity" prop="inQuantity" label="入库量" align="center" min-width="80px" />
        <el-table-column key="unPackageQuantity" prop="unPackageQuantity" label="可打包量" align="center" min-width="80px" />
        <el-table-column prop="productQuantity" label="打包数量" align="center" width="120px" fixed="right">
          <template #default="{ row }">
            <template v-if="!row.boolOneCode">
              <el-input-number
                v-model="row.productQuantity"
                :step="1"
                :min="1"
                :max="row.unPackageQuantity || row.inQuantity - row.packageQuantity"
                size="mini"
                style="width: 100%"
                controls-position="right"
              />
            </template>
            <template v-else>
              <span>{{ row.productQuantity }}</span>
            </template>
          </template>
        </el-table-column>
        <el-table-column label="操作" width="70" align="center" fixed="right">
          <template v-slot="scope">
            <common-button type="danger" icon="el-icon-delete" size="mini" @click.stop="del(scope.row.id)" />
          </template>
        </el-table-column>
      </common-table>
      <el-input
        class="remark-detail"
        v-model.trim="remark"
        type="textarea"
        :autosize="{ minRows: 3, maxRows: 3 }"
        maxlength="255"
        placeholder="请填写备注"
        style="width: 100%; margin-top: 10px"
      />
    </template>
  </common-drawer>
  <chose-add-method-dialog
    ref="choseDialogRef"
    v-model:visible="choseVisible"
    :packType="packType"
    :packLoading="packLoading"
    @handlePack="handlePack"
  />
</template>

<script setup>
import { pack, editPack, additionalPack } from '@/api/mes/pack-and-ship/manual-pack'
import { defineProps, defineEmits, ref, watch, inject, reactive, computed } from 'vue'
import { ElMessage } from 'element-plus'

import { DP } from '@/settings/config'
import { packTypeEnum } from '@enum-ms/mes'
import { toFixed } from '@data-type/index'
import { tableSummary } from '@/utils/el-extra'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import oneCodeNumberList from '@/components-system/mes/one-code-number-list'
import workshopSelect from '@comp-mes/workshop-select'
import choseAddMethodDialog from '../chose-add-method-dialog'
import tableCellTag from '@comp-common/table-cell-tag/index.vue'

const drawerRef = ref()
const choseDialogRef = ref()
const expandRowKeys = ref([])

const packData = inject('packData')
const permission = inject('permission')
const projectId = inject('projectId')
const emit = defineEmits(['update:visible', 'handleSuccess'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  bagId: {
    type: [Number, String],
    default: undefined
  },
  editData: {
    type: Object,
    default: () => {}
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible' })

// 高度
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.drawer-detail',
    extraBox: ['.el-drawer__header', '.head-container', '.remark-detail'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true,
    minHeight: 300,
    extraHeight: 80
  },
  drawerVisible
)

const packType = ref(packTypeEnum.STRUCTURE.V)
const packLoading = ref(false)
const choseVisible = ref(false)
// const factoryId = ref()
const workshopId = ref()
const remark = ref()
const listObj = reactive({
  // [packTypeEnum.STRUCTURE.K]: [],
  // [packTypeEnum.ENCLOSURE.K]: [],
  // [packTypeEnum.AUXILIARY_MATERIAL.K]: [],
  // ['source' + packTypeEnum.STRUCTURE.K]: [],
  // ['source' + packTypeEnum.ENCLOSURE.K]: [],
  // ['source' + packTypeEnum.AUXILIARY_MATERIAL.K]: []
})

const disabledVal = computed(() => {
  const _arr = []
  for (const item in packTypeEnum.ENUM) {
    if (!listObj['source' + item].length) {
      _arr.push(packTypeEnum.KV[item])
    }
  }
  return _arr
})

const totalBadge = computed(() => {
  let _total = 0
  for (const item in packTypeEnum.ENUM) {
    if (listObj[item]?.length) {
      _total += listObj[item].length
    }
  }
  return _total
})

watch(workshopId, (val) => {
  for (const item in packTypeEnum.ENUM) {
    if (listObj['source' + item].length) {
      listObj[item] = listObj['source' + item].filter((v) => val === v.workshop?.id || !val)
    }
  }
})

watch(
  packData,
  () => {
    let _type
    for (const item in packTypeEnum.ENUM) {
      if (packData[item] === null) packData[item] = {}
      listObj[item] = Object.values(packData[item])
      listObj['source' + item] = Object.values(packData[item])
      if (listObj[item].length && !_type) _type = packTypeEnum.KV[item]
    }
    packType.value = _type || packType.value
  },
  { immediate: true, deep: true }
)

watch(
  () => props.bagId,
  (val) => {
    if (val) {
      remark.value = props.editData.remark
    }
  },
  { immediate: true }
)

function packClick() {
  if (props.bagId) {
    handlePack({ bagId: props.bagId })
    return
  }
  choseVisible.value = true
}

function handleSuccess() {
  handleClose()
  remark.value = ''
  packType.value = packTypeEnum.STRUCTURE.V
  workshopId.value = undefined
  // factoryId.value = undefined
}

async function handlePack({ bagId, isNew, selectBagId }) {
  try {
    packLoading.value = true
    const params = {
      remark: remark.value,
      packageLinks: [],
      productType: packType.value,
      projectId: projectId.value
    }
    // 所有类型打包
    for (const item in packTypeEnum.ENUM) {
      const _list = listObj[item]
      for (let i = 0; i < _list.length; i++) {
        const v = _list[i]
        params.packageLinks.push({
          id: v.id,
          numberList: v.numberList,
          quantity: v.productQuantity,
          productType: v.productType
        })
      }
    }
    // 单独类型打包
    // const _list = listObj[packTypeEnum.VK[packType.value]]
    // params.packageLinks = _list.map((v) => {
    //   return {
    //     id: v.id,
    //     numberList: v.numberList,
    //     quantity: v.productQuantity
    //   }
    // })
    if (bagId) {
      params.id = bagId
      if (await editPack(params)) {
        ElMessage({ type: 'success', message: '更新打包清单成功' })
      }
    } else if (isNew) {
      if (await pack(params)) {
        ElMessage({ type: 'success', message: '打包成功' })
      }
    } else {
      params.id = selectBagId
      if (await additionalPack(params)) {
        ElMessage({ type: 'success', message: '追加打包成功' })
      }
    }
    // 关闭选择框，重置选择
    choseDialogRef.value.handleSuccess()
    handleSuccess()
    emit('handleSuccess')
  } catch (error) {
    console.log('手动打包', error)
  } finally {
    packLoading.value = false
  }
}

function oneCodeSelectChange(row) {
  row.productQuantity = row.numberList.length
}

function del(id) {
  delete packData[packTypeEnum.VK[packType.value]][id]
}

function getSummaries(param) {
  return tableSummary(param, { props: ['inQuantity', 'productQuantity'] })
}
</script>

<style>
.manual-pack-list.el-table .el-table__cell.is-hidden > * {
  visibility: visible;
}
</style>

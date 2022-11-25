<template>
  <!-- 套料文件弹窗 -->
  <common-drawer :before-close="handleClose" size="90%" modal append-to-body v-model:visible="nestingFileVisible">
    <template #title>
      <common-radio-button
        style="margin-right: 8px"
        class="filter-item"
        v-model="nestingFileType"
        :options="nestingFileTypeEnum.ENUM"
        type="enum"
        size="small"
        @change="handleChange"
      />
      <export-button
      v-permission="permission.downloadList"
        v-if="nestingFileType === nestingFileTypeEnum.MATERIAL_LIST.V"
        class="filter-item"
        :fn="getMaterialListExcelFn"
        :params="{ id: props.detailData.id }"
        :disabled="nestingProgressData.length === 0"
      >
        材料清单
      </export-button>
      <common-button size="small" @click="handleClose">关闭</common-button>
    </template>
    <template #content>
      <common-table
        v-loading="resultLoading"
        ref="tableRef"
        :data="nestingProgressData"
        :max-height="maxHeight"
        style="width: 100%"
        row-key="id"
      >
        <el-table-column type="expand" v-if="nestingFileType === nestingFileTypeEnum.MATERIAL_LIST.V">
          <template #default="prop">
            <common-table :data="prop.row.linkDOList">
              <el-table-column label="单体" prop="monomerName" align="center" />
              <el-table-column label="区域" prop="areaName" align="center" />
              <el-table-column label="包含部件" prop="serialNumber" align="center" />
              <el-table-column label="长度（mm）" prop="length" align="center" />
              <el-table-column label="重量（kg）" prop="netWeight" align="center" />
              <el-table-column label="数量" prop="quantity" align="center" />
            </common-table>
          </template>
        </el-table-column>
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="套料编号" align="left" width="240px">
          <template v-slot="scope">
            <span>{{ scope.row.serialNumber }}</span>
          </template>
        </el-table-column>
        <el-table-column
          key="nestingResult"
          prop="nestingResult"
          label="套料成果"
          header-align="center"
          v-if="nestingFileType === nestingFileTypeEnum.NESTING_FILE.V"
          width="600px"
        >
          <template v-slot="scope">
            <template v-if="scope.row.linkDOList.length > 0">
              <div style="width: 100%; display: flex">
                <template v-for="item in scope.row.linkDOList" :key="item">
                  <el-tooltip effect="dark" :content="item.serialNumber" placement="top-start">
                    <div
                      class="assemble-shadow"
                      :style="`width:${((item.length / scope.row.assembleLength) * 100).toFixed(2)}%; background-color: ${
                        item.lengthColor
                      }; margin-left: ${(scope.row.kerfLength / scope.row.assembleLength) * 100}% ;margin-right: ${
                        (scope.row.kerfLength / scope.row.assembleLength) * 100
                      }%`"
                    >
                      <!-- 17dh13535487865887486 -->
                      {{ item.serialNumber }}
                    </div>
                  </el-tooltip>
                </template>
                <el-tooltip v-if="scope.row.typesettingTypeEnum === nestingSettingTypeEnum.LOSSY.V" effect="dark" content="余料" placement="top-start">
                  <div class="shadow" style="flex: 1"></div>
                </el-tooltip>
              </div>
            </template>
          </template>
        </el-table-column>
        <el-table-column
          key="typesettingAssembleTypeEnum"
          prop="typesettingAssembleTypeEnum"
          :show-overflow-tooltip="true"
          label="材料属性"
          align="center"
          width="110px"
        >
          <template v-slot="scope">
            <span>{{ scope.row.typesettingAssembleTypeEnum ? materialTypeEnum.VL[scope.row.typesettingAssembleTypeEnum] : '-' }}</span>
          </template>
        </el-table-column>
        <el-table-column key="length" prop="length" :show-overflow-tooltip="true" label="母材长度（mm）" align="center" width="120px">
          <template v-slot="scope">
            <span v-if="scope.row.typesettingTypeEnum === nestingSettingTypeEnum.UN_LOSSY.V">-</span>
            <span v-else>{{ scope.row.length }}</span>
          </template>
        </el-table-column>
        <el-table-column
          key="specification"
          prop="specification"
          :show-overflow-tooltip="true"
          label="母材规格"
          align="center"
          width="200px"
        >
          <template v-slot="scope">
            <span>{{ scope.row.specification }}</span>
          </template>
        </el-table-column>
        <el-table-column key="material" prop="material" :show-overflow-tooltip="true" label="材质" align="center">
          <template v-slot="scope">
            <span>{{ scope.row.material }}</span>
          </template>
        </el-table-column>
        <el-table-column key="netWeight" prop="netWeight" :show-overflow-tooltip="true" label="母材总重" align="center" width="100px">
          <template v-slot="scope">
            <span v-if="scope.row.typesettingTypeEnum === nestingSettingTypeEnum.UN_LOSSY.V">-</span>
            <span v-else>{{ scope.row.netWeight }}</span>
          </template>
        </el-table-column>
        <el-table-column key="quantity" prop="quantity" :show-overflow-tooltip="true" label="数量" align="center" width="60px">
          <template v-slot="scope">
            <span>{{ scope.row.quantity }}</span>
          </template>
        </el-table-column>
        <el-table-column
          key="typesettingLength"
          prop="typesettingLength"
          :show-overflow-tooltip="true"
          label="套料长度（mm）"
          align="center"
          width="120px"
        >
          <template v-slot="scope">
            <span>{{ scope.row.typesettingLength }}</span>
          </template>
        </el-table-column>
        <el-table-column key="lossRate" prop="lossRate" :show-overflow-tooltip="true" label="损耗" align="center" width="70px">
          <template v-slot="scope">
            <span>{{ scope.row.lossRate }}%</span>
          </template>
        </el-table-column>
        <el-table-column  v-if="nestingFileType === nestingFileTypeEnum.NESTING_FILE.V" key="statusEnum" prop="statusEnum" :show-overflow-tooltip="true" label="状态" align="center" fixed="right" width="100px">
          <template v-slot="scope">
            <el-tag :type="typeEnum.V[scope.row.statusEnum].T">{{ typeEnum.VL[scope.row.statusEnum] }}</el-tag>
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script  setup>
import useVisible from '@compos/use-visible'
import useMaxHeight from '@compos/use-max-height'
import { getLightColor } from '@/utils/color'
import { nestingProgress } from '@/api/mes/craft-manage/section-steel/nesting-setting'
import { getMaterialList, getMaterialListExcelFn } from '@/api/mes/craft-manage/section-steel/nesting-result'
import { ref, defineProps, defineEmits } from 'vue'
import { mesNestingResultPM as permission } from '@/page-permission/mes'
import { nestingFileTypeEnum, mesBuildingTypeSettingAssembleTypeEnum as materialTypeEnum, nestingSettingTypeEnum, MesBuildingTypesettingStatusEnum as typeEnum } from '@enum-ms/mes'
import ExportButton from '@comp-common/export-button/index.vue'

const nestingProgressData = ref([])
const resultLoading = ref(false)
const nestingFileType = ref(nestingFileTypeEnum.NESTING_FILE.V)
const emit = defineEmits(['success'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  detailData: {
    type: Object,
    default: () => {}
  }
})
const { visible: nestingFileVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: showHook })

function showHook() {
  nestingFileType.value = nestingFileTypeEnum.NESTING_FILE.V
  nestingResultGet()
}
const colorObj = ref({}) // serialNumber: color

// 套料文件
async function nestingResultGet() {
  try {
    resultLoading.value = true
    const { content } = await nestingProgress({ batchId: props.detailData.id })
    content[0].typesettingDTOS.forEach((v) => {
      v.assembleLength = v.typesettingTypeEnum === nestingSettingTypeEnum.UN_LOSSY.V ? 0 : v.length
      v.linkDOList.map((m) => {
        if (!colorObj.value[m.serialNumber]) {
          colorObj.value[m.serialNumber] = getLightColor()
        }
        m.lengthColor = colorObj.value[m.serialNumber]
        if (v.typesettingTypeEnum === nestingSettingTypeEnum.UN_LOSSY.V) {
          v.assembleLength += m.length
        }
      })
    })
    nestingProgressData.value = content[0].typesettingDTOS
  } catch (error) {
    console.log('获取套料文件失败')
  } finally {
    resultLoading.value = false
  }
}

// 材料清单
async function nestingListGet() {
  try {
    resultLoading.value = true
    const { content } = await getMaterialList({ id: props.detailData.id })
    console.log(content)
    nestingProgressData.value = content[0].typesettingDTOS
  } catch (error) {
    console.log('获取材料清单失败')
  } finally {
    resultLoading.value = false
  }
}

function handleChange(val) {
  if (val === nestingFileTypeEnum.MATERIAL_LIST.V) {
    nestingListGet()
  } else {
    nestingResultGet()
  }
}

// 高度
const { maxHeight } = useMaxHeight({
  mainBox: '.common-drawer',
  extraBox: ['.el-drawer__header'],
  wrapperBox: ['.el-drawer__body'],
  navbar: false,
  clientHRepMainH: true,
  minHeight: 300
})
</script>

<style lang="scss" scope>
.shadow {
  background: linear-gradient(135deg, #ffffff 25%, #5588aa 0, #5588aa 50%, #ffffff 0, #ffffff 75%, #5588aa 0);
  background-size: 15px 15px;
  color: #dbfaff;
  height: 30px;
  display: inline-block;
}
.assemble-shadow {
  padding: 0 5px;
  display: inline-block;
  color: #fff;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
  height: 30px;
}
</style>

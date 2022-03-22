<template>
  <div class="app-container">
    <template v-if="globalProject && globalProject.projectContentList && globalProject.projectContentList.length>0">
      <!--工具栏-->
      <common-radio-button
        v-model="queryProductType"
        :options="currentOption"
        :type="'other'"
        :dataStructure="typeProp"
        @change="typeChange"
        style="margin-bottom:10px;"
      />
      <div style="margin-bottom:10px;" v-if="queryProductType!==7 && queryProductType!==9">
        <monomer-select
          ref="monomerSelectRef"
          v-model="monomerId"
          :project-id="globalProject.id"
          class="filter-item"
          @change="getEnclosureData"
        />
      </div>
      <!--表格渲染-->
      <common-table
        v-if="queryProductType===7 || queryProductType===9"
        v-loading="loading"
        ref="tableRef"
        :data="tableData"
        :empty-text="checkPermission(permission.get)?'暂无数据':'暂无权限'"
        :max-height="maxHeight"
        :showEmptySymbol="false"
        style="width: 100%"
      >
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column key="name" prop="name" :show-overflow-tooltip="true" label="单体" width="200px">
        <template v-slot="scope">
          <span>{{ scope.row.name }}</span>
        </template>
      </el-table-column>
      <el-table-column key="quantity" prop="quantity" :show-overflow-tooltip="true" label="合计数量" width="200px">
        <template v-slot="scope">
          <span>{{ scope.row.quantity }}</span>
        </template>
      </el-table-column>
      <el-table-column key="totalNetWeight" prop="totalNetWeight" :show-overflow-tooltip="true" label="合计毛重(kg)" width="200px" v-if="!enclosureCategory">
        <template v-slot="scope">
          <span>{{ scope.row.totalGrossWeight?scope.row.totalGrossWeight.toFixed(DP.COM_WT__KG):'-' }}</span>
        </template>
      </el-table-column>
      <el-table-column key="totalGrossWeight" prop="totalGrossWeight" :show-overflow-tooltip="true" label="合计净重(kg)" width="200px" v-if="!enclosureCategory">
        <template v-slot="scope">
          <span>{{ scope.row.totalNetWeight?scope.row.totalNetWeight.toFixed(DP.COM_WT__KG):'-' }}</span>
        </template>
      </el-table-column>
      <el-table-column key="totalLength" prop="totalLength" :show-overflow-tooltip="true" label="合计量(m)" width="200px" v-if="enclosureCategory">
        <template v-slot="scope">
          <span>{{ scope.row.totalLength?scope.row.totalLength.toFixed(DP.COM_L__M):'-' }}</span>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column
        label="操作"
        align="left"
        fixed="right"
      >
        <template v-slot="scope">
          <common-button size="mini" type="primary" @click="openDetail(scope.row)" v-permission="permission.get">查看区域详情</common-button>
        </template>
      </el-table-column>
    </common-table>
    <enclosureTable v-else :enclosureData="enclosureData" :category="enclosureCategory" :blankText="checkPermission(permission.get)?'暂无数据':'暂无权限'"/>
    <mDetail :current-info="currentInfo" v-model="detailVisible" :globalProject="globalProject" :enclosureCategory="enclosureCategory"/>
    </template>
  </div>
</template>

<script setup>
import { getStructure, getPart, getEnclosure, structureMonomer, partMonomer } from '@/api/plan/technical-manage/summary-list'
import { ref, watch } from 'vue'
import { summaryListPM as permission } from '@/page-permission/plan'
import checkPermission from '@/utils/system/check-permission'
import useMaxHeight from '@compos/use-max-height'
import { mapGetters } from '@/store/lib'
import mDetail from './module/detail'
import enclosureTable from './module/enclosure-table'
import { isNotBlank } from '@data-type/index'
import { TechnologyTypeAllEnum, businessTypeEnum } from '@enum-ms/contract'
import monomerSelect from '@/components-system/plan/monomer-select'
import { DP } from '@/settings/config'

const { globalProject } = mapGetters(['globalProject'])

const tableRef = ref()
const currentInfo = ref([])
const detailVisible = ref(false)
const loading = ref(false)
const monomerId = ref()

const { maxHeight } = useMaxHeight({
  wrapperBox: '.summary-list',
  paginate: true,
  extraHeight: 40
})

const queryProductType = ref()
const enclosureCategory = ref()
const typeProp = { key: 'no', label: 'name', value: 'no' }
const currentOption = ref([])
const tableData = ref([])
const enclosureData = ref([])
const techOptions = [
  { name: '构件', key: 'mainStructure', dateKey: 'mainStructureDate', no: TechnologyTypeAllEnum.STRUCTURE.V, alias: 'STRUCTURE' },
  {
    name: '夹芯板',
    key: 'battenBoard',
    dateKey: 'battenBoardDate',
    no: TechnologyTypeAllEnum.SANDWICH_BOARD.V,
    alias: 'ENCLOSURE'
  },
  {
    name: '压型彩板',
    key: 'contourPlate',
    dateKey: 'contourPlateDate',
    no: TechnologyTypeAllEnum.PROFILED_PLATE.V,
    alias: 'ENCLOSURE'
  },
  {
    name: '折边件',
    key: 'flangingPiece',
    dateKey: 'flangingPieceDate',
    no: TechnologyTypeAllEnum.BENDING.V,
    alias: 'ENCLOSURE'
  },
  {
    name: '桁架楼承板',
    key: 'trussFloorPlate',
    dateKey: 'trussFloorPlateDate',
    no: TechnologyTypeAllEnum.TRUSS_FLOOR_PLATE.V,
    alias: 'ENCLOSURE'
  },
  {
    name: '压型楼承板',
    key: 'pressureBearingPlate',
    dateKey: 'pressureBearingPlateDate',
    no: TechnologyTypeAllEnum.PRESSURE_BEARING_PLATE.V,
    alias: 'ENCLOSURE'
  }
]
watch(
  () => globalProject.value,
  (val) => {
    if (isNotBlank(val)) {
      currentOption.value = []
      val.projectContentList.forEach((v) => {
        if (val.businessType === businessTypeEnum.ENUM.MACHINING.V) {
          if (v.no && techOptions.findIndex((k) => k.no === Number(v.no)) > -1 && Number(v.no) !== TechnologyTypeAllEnum.STRUCTURE.V) {
            const optionVal = techOptions.find((k) => k.no === Number(v.no))
            currentOption.value.push(optionVal)
          }
        } else if (val.businessType === businessTypeEnum.ENUM.INSTALLATION.V) {
          if (v.childrenList && v.childrenList.length > 0) {
            v.childrenList.forEach((value) => {
              if (value.no && techOptions.findIndex((k) => k.no === Number(value.no)) > -1 && Number(value.no) !== TechnologyTypeAllEnum.STRUCTURE.V) {
                const optionVal = techOptions.find((k) => k.no === Number(value.no))
                currentOption.value.push(optionVal)
              }
            })
          }
        }
      })
      if (currentOption.value.findIndex((k) => k.alias === 'ENCLOSURE') > -1) {
        const optionVal = {
          name: '折边件',
          key: 'flangingPiece',
          dateKey: 'flangingPieceDate',
          alias: 'ENCLOSURE',
          no: TechnologyTypeAllEnum.BENDING.V
        }
        currentOption.value.push(optionVal)
      }
      currentOption.value.unshift(
        {
          name: '构件',
          no: 7
        },
        { name: '零件', no: 9 }
      )
      queryProductType.value = currentOption.value.length > 0 ? currentOption.value[0].no : undefined
      typeChange(queryProductType.value)
    }
  },
  { deep: true, immediate: true }
)

function typeChange(val) {
  tableData.value = []
  enclosureCategory.value = undefined
  if (val) {
    if (!checkPermission(permission.get)) {
      return
    }
    switch (val) {
      case 7:
        getStructureData()
        break
      case 9:
        getPartData()
        break
      default:
        enclosureCategory.value = queryProductType.value
        getEnclosureData()
        break
    }
  }
}
async function getStructureData() {
  loading.value = true
  enclosureData.value = []
  try {
    const { content } = await getStructure({ projectId: globalProject.value.id })
    if (content.length > 0) {
      content.forEach(v => {
        v.monomerId = v.monomer.id
        v.name = v.monomer.name
      })
    }
    tableData.value = content
  } catch (e) {
    tableData.value = []
    console.log('获取构件清单', e)
  } finally {
    loading.value = false
  }
}

async function getPartData() {
  loading.value = true
  enclosureData.value = []
  try {
    const { content } = await getPart({ projectId: globalProject.value.id })
    if (content.length > 0) {
      content.forEach(v => {
        v.monomerId = v.monomer.id
        v.name = v.monomer.name
      })
    }
    tableData.value = content
  } catch (e) {
    tableData.value = []
    console.log('获取零件清单', e)
  } finally {
    loading.value = false
  }
}

async function getEnclosureData() {
  if (!checkPermission(permission.get)) {
    return
  }
  loading.value = true
  tableData.value = []
  enclosureData.value = []
  try {
    const { content } = await getEnclosure({ projectId: globalProject.value.id, category: enclosureCategory.value, monomerId: monomerId.value })
    enclosureData.value = content || []
  } catch (e) {
    console.log('获取围护清单', e)
  } finally {
    loading.value = false
  }
}

async function getStructureMonomer(id) {
  try {
    const { content } = await structureMonomer({ projectId: globalProject.value.id, monomerId: id })
    if (content.length > 0) {
      content.forEach(v => {
        v.areaId = v.area.id
        v.name = v.area.name
        v.axis = v.area.axis
      })
    }
    currentInfo.value = content
  } catch (e) {
    currentInfo.value = []
    console.log('获取构件单体清单', e)
  }
}

async function getPartMonomer(id) {
  try {
    const { content } = await partMonomer({ projectId: globalProject.value.id, monomerId: id })
    if (content.length > 0) {
      content.forEach(v => {
        v.areaId = v.area.id
        v.name = v.area.name
        v.axis = v.area.axis
      })
    }
    currentInfo.value = content
  } catch (e) {
    currentInfo.value = []
    console.log('获取零件单体清单', e)
  }
}

// async function getEnclosureMonomer(id) {
//   try {
//     const { content } = await enclosureMonomer({ projectId: globalProject.value.id, category: enclosureCategory.value, monomerId: id })
//     if (content.length > 0) {
//       content.forEach(v => {
//         v.areaId = v.area.id
//         v.name = v.area.name
//         v.axis = v.area.axis
//       })
//     }
//     currentInfo.value = content
//   } catch (e) {
//     currentInfo.value = []
//     console.log('获取围护单体清单', e)
//   }
// }

function openDetail(row) {
  currentInfo.value = []
  if (queryProductType.value) {
    switch (queryProductType.value) {
      case 7:
        getStructureMonomer(row.monomerId)
        break
      case 9:
        getPartMonomer(row.monomerId)
        break
      default:
        // getEnclosureMonomer(row.monomerId)
        break
    }
  }
  detailVisible.value = true
}
</script>

<style lang="scss" scoped>
::v-deep(.abnormal-row) {
  background: #e8f4ff;
}
::v-deep(.hidden-select) {
  td:nth-child(1){
    .cell{
      opacity:0;
    }
  }
}
$font-size: 1.5em;
.child {
  width: $font-size;
  height: $font-size;
  display: inline-block;
  border: 1px solid;
  border-radius: 50%;
  line-height: $font-size;
}
</style>

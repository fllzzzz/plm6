<template>
  <crudOperation>
    <div v-show="crud.props.searchToggle" slot="optLeft">
      <common-radio-button
        v-enclosure="{field:'query.type'}"
        :value.sync="query.type"
        :options="typeEnum"
        :unshow-val="[typeEnum.AUXILIARY_MATERIAL.V, typeEnum.ARTIFACT_TREE.V]"
        type="enum"
        class="filter-item"
        @change="crud.toQuery"
      />
    </div>
  </crudOperation>
</template>

<script>
import { header } from '@crud/crud'
import crudOperation from '@crud/CRUD.operation'
import { materialListTypeEnum as typeEnum } from '@/utils/enum/index'

const defaultQuery = {
  type: { value: typeEnum.ARTIFACT.V, resetAble: false },
  projectId: { value: typeEnum.ARTIFACT.V, resetAble: false }
}
export default {
  components: { crudOperation },
  mixins: [header(defaultQuery)],
  inject: ['permission'],
  props: {
    projectId: {
      type: Number,
      required: true
    }
  },
  data() {
    return {
      typeEnum
    }
  },
  watch: {
    projectId(val) {
      this.query.projectId = val
      this.crud.toQuery()
    }
  },
  created() {
    this.query.projectId = this.projectId
  }
}
</script>

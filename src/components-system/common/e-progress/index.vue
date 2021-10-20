<!-- 进度条 -->
<template>
  <!-- :format="format(scope.row.completionRate)" 目前不可使用 -->
  <el-progress
    :type="type"
    :status="status"
    :text-inside="textInside"
    :width="width"
    :stroke-width="strokeWidth"
    :percentage="progressPercentage"
    :format="() => {return format(percentage)}"
    :color="color"
  />
</template>

<script>
export default {
  props: {
    textInside: {
      type: Boolean,
      default: false
    },
    width: {
      type: Number,
      default: 126
    },
    strokeWidth: {
      type: Number,
      default: 6
    },
    percentage: {
      type: [Number, String],
      default: 0
    },
    // eslint-disable-next-line vue/require-default-prop
    color: {
      type: String
    },
    status: {
      type: String,
      default: undefined
    },
    type: {
      type: String,
      default: 'line'
    }
  },
  computed: {
    progressPercentage() {
      const percentage = +this.percentage
      return percentage > 100 ? 100 : percentage
    }
  },
  methods: {
    format(percentage) {
      return `${percentage}%`
    }
  }
}
</script>
